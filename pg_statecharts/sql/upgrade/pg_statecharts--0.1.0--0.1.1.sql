\echo Use "ALTER EXTENSION pg_statecharts UPDATE" to load this file. \quit

-- Upgrade from 0.1.0 to 0.1.1.
--
-- Events are handed to fsm.handle_machine_events by the statement that
-- inserts them instead of being looked up there. See
-- sql/function/handle_machine_events.sql for why.
--
-- Everything here is additive: a new type, a new overload, two replaced
-- bodies and a column default. Nothing is dropped, so a database that is
-- mid-flight through this upgrade keeps working either way.

-- fsm.machine_event may already exist. The change this release ports was
-- first deployed as a sqitch change in the application that vendors this
-- extension, so that database already has the type, created outside the
-- extension. Create it when it is missing, adopt it when it matches, and
-- refuse when it does not: silently compiling the functions below against a
-- type of a different shape is the one outcome worth failing loudly over.
do $machine_event$
declare
  found text;
  wanted constant text :=
    'shard_id bigint, id bigint, state_machine_id bigint, '
    'created_at timestamp with time zone, name text, data jsonb';
begin
  select string_agg(a.attname || ' ' || format_type(a.atttypid, a.atttypmod), ', ' order by a.attnum)
  into found
  from pg_type t
  join pg_namespace n on n.oid = t.typnamespace
  join pg_attribute a on a.attrelid = t.typrelid and a.attnum > 0 and not a.attisdropped
  where n.nspname = 'fsm' and t.typname = 'machine_event'
  group by t.oid;

  if found is null then
    create type fsm.machine_event as
    ( shard_id bigint
    , id bigint
    , state_machine_id bigint
    , created_at timestamptz
    , name text
    , data jsonb
    );
  elsif found <> wanted then
    raise exception 'fsm.machine_event already exists with a different definition'
      using detail = format('found (%s), expected (%s)', found, wanted),
            hint   = 'drop it, or reconcile it with this release, and run the update again';
  elsif not exists (
      select 1 from pg_depend d
      join pg_type t on t.oid = d.objid
      join pg_namespace n on n.oid = t.typnamespace
      where d.classid = 'pg_type'::regclass
        and d.refclassid = 'pg_extension'::regclass
        and d.deptype = 'e'
        and n.nspname = 'fsm' and t.typname = 'machine_event'
  ) then
    -- It matches but belongs to nobody, so take it over: otherwise this
    -- database owns one object fewer than a fresh install of the same
    -- version, and dropping the extension would leave the type behind.
    alter extension pg_statecharts add type fsm.machine_event;
  end if;
end
$machine_event$;

comment on type fsm.machine_event is
    'An event as fsm.handle_machine_events receives it.';

-- Handles `events`, all of them for this machine, in the order given.
--
-- They are passed in rather than looked up because the trigger that calls this
-- already has them. notify_machine_event fires at the end of every statement
-- that inserts an event, including an insert made from inside a transition
-- callback, so the rows in its transition table are exactly the machine's
-- unhandled events. Not looking them up again keeps the event table
-- write-only on this path.
--
-- That matters under serializable. Looking the events up and then marking them
-- handled put a read lock and a fresh index entry on the same few pages in
-- every concurrent transaction: the update could never be HOT, because
-- idx_state_machine names handled_at, so every event added an entry on the
-- right edge of the index that every other transaction had just read. Machines
-- with nothing to do with each other kept aborting each other with 40001.
-- handled_at is stamped by its column default instead; see
-- sql/table/state_machine_event.sql.
create or replace function fsm.handle_machine_events(
    shard bigint
  , machine_id bigint
  , events fsm.machine_event[]
)
  returns void as
$$
  declare
    handled fsm.machine_event;
    state_transition record;
    on_exit_function_call record;
    on_entry_function_call record;
  begin

    -- For each event, select the set of source active states and future target
    -- states based on the event transition. This selection will be used to
    -- perform the transition effects and record the new machine current states.
    -- An event with no valid transition is simply dropped.
    for handled in
        select *
        from unnest(events)
        order by created_at, id
    loop
      for state_transition in

        -- {{{transition-query
        select
            source as source_state
          , target as target_state
          , coalesce(source_tree.states, array[]::fsm.state[]) as children_source_states
          , coalesce(target_tree.states, array[]::fsm.state[]) as children_target_states
          , target.is_final as target_is_final

        -- we need to lookup the states the machine is at. Since this will
        -- yield multiple states, we will need to filter the relevant ones
        from fsm.state_machine_state as current_state

        -- this filters the currently active states down to the list of states
        -- that accept the named transition. The effect of this join should be
        -- that only one source state is matched per named event
        join fsm.transition transition
          on  transition.statechart_id = current_state.statechart_id
          and handled.name = transition.event
          and transition.source_state = current_state.state_id

        -- we need to remember certain details of the source state we are exiting,
        -- for example, if it is final we need to generate a "done" event for it.
        join fsm.state source
          on  source.statechart_id = transition.statechart_id
          and source.id = current_state.state_id

        -- likewise, we remeber the details for the target state we need to activate.
        -- so that we can execute the `on_entry` callback.
        join fsm.state target
          on  target.statechart_id = transition.statechart_id
          and target.id = transition.target_state

        -- we need to get the entire sub-tree for the source state we are transitioning
        -- from, so that we can call all of the on_exit callbacks.
        join lateral (
          select array_agg(st::fsm.state)::fsm.state[] as states from
          ( select state_child.*
            from fsm.state state_child
            -- we only want child states that are currently activated in the state machine
            -- so, we filter by `exited_at is null` and correlate to the current
            -- state_child we are inspecting
            join fsm.state_machine_state as current_children_state
              on current_children_state.shard_id = shard
              and current_children_state.state_machine_id = machine_id
              and current_children_state.state_id = state_child.id
              and current_children_state.exited_at is null
            where state_child.statechart_id = current_state.statechart_id
              and state_child.parent_path <@ source.node_path
          ) st
        ) source_tree on true

        -- Similarly, we also need the tree for the target state, as we need to
        -- activate all the `is_initial` states and execute their `on_entry` callbacks
        join lateral (
          select array_agg(tt::fsm.state)::fsm.state[] as states from
           ( select state_child.*
             from fsm.state state_child
             where state_child.statechart_id = current_state.statechart_id
               and state_child.parent_path <@ target.node_path
               and state_child.is_initial
               -- We have selected so far all initial children recursively, but we have
               -- now to filter out those who have parents that are not initial as well
               -- Any node in the chain that is not active should break the chain of nodes
               -- that we are activating recursively.
               and not exists (
                   select *
                   from fsm.state bad_parent
                   where bad_parent.statechart_id = current_state.statechart_id
                      and target.node_path @> bad_parent.node_path
                      and bad_parent.node_path @> state_child.parent_path
                      and bad_parent.id <> target.id
                      and not bad_parent.is_initial
               )
          ) tt
        ) target_tree on true

        where current_state.shard_id = shard
          and current_state.state_machine_id = machine_id
          and current_state.exited_at is null
      -- transition-query}}}

      loop

        -- For all the active source states, we mark them as exited and execute their respective
        -- on_exit callback if any
        -- {{{source-exiting
        for on_exit_function_call in
          with

            target_states as (
              select *
              from
                unnest(state_transition.target_state || state_transition.children_target_states)
            )

          , source_states as (
              select *
              from
                unnest(state_transition.children_source_states || state_transition.source_state)
            )

          , updated_states as (
              update fsm.state_machine_state
              /* using clock_timestamp() instead of now(). now() uses
               * transaction time which can cause problem when another process
               * adds fsm.state_machine_state rows during the transaction. This
               * can cause the exited_at time to be earlier than the entered
               * time causing a constraint errors
               */
              set exited_at = clock_timestamp()
              where shard_id = shard
                and state_machine_id = machine_id
                and state_id in (select a.id from source_states a)
                -- if we are looping into the same state, we don't want to mark the current state as exited
                and state_id not in (select a.id from target_states a)
                and exited_at is null
            )

          select shard, machine_id, machine_id, id, unnest(on_exit) as on_exit from source_states
        loop
          execute format('select %I.%I(%L::fsm_event_payload)',
              coalesce((on_exit_function_call.on_exit).schema_name, 'public')
            , (on_exit_function_call.on_exit).function_name
            , ( shard
              , machine_id
              , handled.name
              , handled.data
              , on_exit_function_call.id
              , (state_transition.target_state).id
              , 'on_exit'
              )::fsm_event_payload
            );
         end loop;
        -- source-exiting}}}

        -- now we do the same for the target states, but in reverse order. We enter states from the
        -- outermost to the innermost and execute their on_exit callback if any.
        -- {{{target-entering
        for on_entry_function_call in
          with

            target_states as (
              select *
              from
                unnest(state_transition.target_state || state_transition.children_target_states)
            )

          , source_states as (
              select *
              from
                unnest(state_transition.children_source_states || state_transition.source_state)
            )

          , new_states as (
              insert into fsm.state_machine_state (shard_id, state_machine_id, statechart_id, state_id)
              select
                  shard as shard_id
                , machine_id as state_machine_id
                , a.statechart_id
                , a.id as state_id
              from target_states a
              -- We allow transitioning back to the same state (a loop)
              -- but for storage reasons, we prefer not to insert those rows again
              -- here, we exclude target states that are identical to the source state
              where a.id not in (
                select x.id from source_states x
              )
            )

          select shard, machine_id, machine_id, id, unnest(on_entry) as on_entry from target_states
        loop
          execute format('select %I.%I(%L::fsm_event_payload)',
              coalesce((on_entry_function_call.on_entry).schema_name, 'public')
            , (on_entry_function_call.on_entry).function_name
            , ( shard
              , machine_id
              , handled.name
              , handled.data
              , (state_transition.source_state).id
              , on_entry_function_call.id
              , 'on_entry'
              )::fsm_event_payload);
        end loop;
        -- target-entering}}}

        -- generate the "done" event for the target state if it is_final,
        -- including the parents, if needed
        -- {{{event-generation
        if state_transition.target_is_final
        then
          insert into fsm.state_machine_event (shard_id, state_machine_id, name, data) values
            ( shard
            , machine_id
            , format('done.state.%s', (state_transition.target_state).id)
            , handled.data
            );

          insert into fsm.state_machine_event (shard_id, state_machine_id, name, data)
            select shard as shard_id, machine_id as state_machine_id
                 , format('done.state.%s', f.id)
                 , handled.data as data
            from fsm.get_finalized_parents(
                shard
              , machine_id
              , (state_transition.target_state).parent_path
            ) f;
        end if;
        -- event-generation}}}

      end loop;

    end loop;

    return;
  end;
$$ language plpgsql volatile strict;

-- The queue-reading form: looks up whatever is still unhandled for the
-- machine, marks it, and hands it to the function above.
--
-- Events sent with fsm.notify_state_machine are already handled by the time it
-- returns, because the insert fires notify_machine_event. This form is for
-- events inserted with that trigger disabled, which have to set handled_at to
-- null explicitly now that the column defaults to now().
create or replace function fsm.handle_machine_events(shard bigint, machine_id bigint)
  returns void as
$$
  declare
    events_ fsm.machine_event[];
  begin
    with unhandled as (
        select *
        from fsm.state_machine_event
        where shard_id = shard
          and state_machine_id = machine_id
          and handled_at is null
      )
    , handling as (
        update fsm.state_machine_event set handled_at = now()
        where (shard_id, id) in (select shard_id, id from unhandled)
      )
    select array_agg(
        (shard_id, id, state_machine_id, created_at, name, data)::fsm.machine_event
        order by created_at, id
    )
    into events_
    from unhandled;

    if events_ is not null then
      perform fsm.handle_machine_events(shard, machine_id, events_);
    end if;

    return;
  end;
$$ language plpgsql volatile strict;

-- This is an AFTER INSERT ... FOR EACH STATEMENT trigger, and it fires at the
-- end of every statement that inserts an event, including an insert made from
-- inside a transition callback. So the rows in `new` are all of the machine's
-- unhandled events: an event is never left for a later statement to pick up.
-- That is what lets the rows be handed straight to fsm.handle_machine_events
-- instead of being looked up there; see that function for why it matters.
create or replace function fsm.trig_notify_machine_event() returns trigger as
$$
  declare
    machine_ record;
  begin

    for machine_ in
        -- Aliased: a bare `new` would clash with PL/pgSQL's own NEW record.
        select
            inserted.shard_id
          , inserted.state_machine_id
          , array_agg(
                ( inserted.shard_id
                , inserted.id
                , inserted.state_machine_id
                , inserted.created_at
                , inserted.name
                , inserted.data
                )::fsm.machine_event
                order by inserted.created_at, inserted.id
            ) as events
        from new as inserted
        group by inserted.shard_id, inserted.state_machine_id
    loop
      perform fsm.handle_machine_events(
          machine_.shard_id
        , machine_.state_machine_id
        , machine_.events
      );
    end loop;

    return null;
  end;
$$ language plpgsql;

comment on function fsm.trig_notify_machine_event is $comment$
  Handles all of the newly inserted events for all affected state machines.
$comment$;

-- Every event is handled by the statement that inserts it, so it is handled as
-- of its insert. This replaces the update that used to mark it afterwards.
alter table fsm.state_machine_event alter column handled_at set default now();

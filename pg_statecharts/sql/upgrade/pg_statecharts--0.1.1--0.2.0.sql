\echo Use "ALTER EXTENSION pg_statecharts UPDATE" to load this file. \quit

-- Upgrade from 0.1.1 to 0.2.0.
--
--     alter extension pg_statecharts update;
--
-- Two things change, and both are about the arguments these functions take.
--
--   1. shard_id is optional and comes last. It used to be the first argument
--      of almost every function, which made every caller pass a 1 through it
--      whether or not the database was sharded. It now defaults to 1, and so
--      the common call is fsm.start_machine(machine_id => ...).
--
--   2. The argument names are consistent. shard_id had been spelled shard,
--      shard_id_, shid and shard_id in different functions; the machine had
--      been machine_id, machine and smid; the chart name had been named. The
--      names are now shard_id, machine_id, statechart_id, statechart_name,
--      event_name and state_id everywhere.
--
-- Callers that name their arguments do not have to change with this upgrade.
-- Every old name is kept as a deprecated parameter that still resolves, so
-- fsm.is_state_active(shid => 1, smid => 42, state => 'on') keeps working
-- alongside fsm.is_state_active(machine_id => 42, state_id => 'on'). The
-- deprecated names are removed in 0.3.0, so this is the release in which to
-- move over. They are listed last in each signature and marked in the source.
--
-- Callers that pass their arguments positionally DO have to change, and all
-- but two of them fail loudly, because moving shard_id to the end also
-- changes the sequence of types. The two exceptions take two bigints before
-- and after:
--
--     fsm.create_machine(1, 42)           -- was (shard, statechart)
--     fsm.handle_machine_events(1, 42)    -- was (shard, machine_id)
--
-- Those two still resolve and now mean the opposite of what they did. Grep for
-- them before upgrading.
--
-- Because the deprecated names have to default to null to be omittable, the
-- functions that were declared strict cannot be any more: a defaulted null
-- argument would make a strict function return null without running. Each one
-- reproduces what strict did -- null in, null or no rows out -- in its own
-- body instead, so the behaviour a caller sees is unchanged. 0.3.0 restores
-- the strict declarations along with removing the deprecated names.
--
-- fsm.handle_machine_events gained a three argument form in 0.1.1, taking the
-- events to handle rather than looking them up. 0.1.1 was never released on
-- its own, so nothing has ever called that form by another spelling: it is
-- renamed here with no deprecated parameters. Its argument types move too, so
-- it is dropped and recreated along with the rest.
--
-- fsm.is_state_active also stops reporting states the machine has already left
-- as active. It read fsm.state_machine_state without filtering on exited_at,
-- so it answered "has this machine ever entered this state".

-- A parameter list is part of a function's identity, so each of these has to
-- be dropped and recreated rather than replaced. Dropping an extension's own
-- objects is refused everywhere except inside its update script, which is
-- where this runs.
drop function fsm.create_machine(bigint, bigint);
drop function fsm.create_state_machine_with_latest_statechart(bigint, text);
drop function fsm.get_finalized_parents(bigint, bigint, ltree);
drop function fsm.get_initial_state(bigint);
drop function fsm.get_latest_statechart(text);
drop function fsm.handle_machine_events(bigint, bigint);
drop function fsm.handle_machine_events(bigint, bigint, fsm.machine_event[]);
drop function fsm.is_state_active(bigint, bigint, text);
drop function fsm.is_valid_transition(bigint, bigint, text);
drop function fsm.notify_state_machine(bigint, bigint, text, jsonb);
drop function fsm.start_machine(bigint, bigint, jsonb);
drop function fsm.start_machine_with_latest_statechart(bigint, text, jsonb);

-- sql/function/get_latest_statechart.sql
-- `named` is the name this took before 0.2.0, kept so that callers that name
-- their arguments keep working, and removed in 0.3.0.
create or replace function fsm.get_latest_statechart(
    statechart_name text default null
  -- deprecated, removed in 0.3.0
  , named           text default null
)
returns setof fsm.statechart as
$$
    select *
    from fsm.statechart
    -- a null name matches nothing, which is the zero rows `strict` used to
    -- return before the deprecated name forced a default on
    where name = coalesce(statechart_name, named)
    -- fsm.semver is an integer array, so this is already a numeric comparison
    -- component by component: 1.10.0 outranks 1.9.0 because 10 > 9.
    order by version desc
    limit 1
$$ language sql
    stable
    parallel safe
    rows 1;

comment on function fsm.get_latest_statechart(text, text) is $comment$
    Get the latest statechart of given name.
$comment$;

-- sql/function/create_machine.sql
-- shard_id is last and defaulted: most databases are not sharded, so the
-- common call is fsm.create_machine(statechart_id => ...).
--
-- The trailing parameters are the names this function used before 0.2.0. They
-- are kept so that callers that name their arguments keep working across the
-- upgrade, and they are removed in 0.3.0. coalesce reads the old name first
-- because the new one carries a default that would otherwise always win.
create or replace function fsm.create_machine(
    statechart_id bigint default null
  , shard_id      bigint default 1
  -- deprecated, removed in 0.3.0
  , shard         bigint default null
  , statechart    bigint default null
)
  returns fsm.state_machine as
  $$
      -- The deprecated names have to default to null to be omittable, which
      -- rules out `strict`. The where clause takes its place: a null id yields
      -- no row, and the function returns null, as it did when it was strict.
      insert into fsm.state_machine (shard_id, statechart_id)
      select coalesce(shard, create_machine.shard_id)
           , coalesce(create_machine.statechart_id, statechart)
      where coalesce(shard, create_machine.shard_id) is not null
        and coalesce(create_machine.statechart_id, statechart) is not null
      returning *;
  $$ language sql volatile;

comment on function fsm.create_machine(bigint, bigint, bigint, bigint) is $comment$
    creates a state machine without starting it
$comment$;

-- sql/function/create_state_machine_with_latest_statechart.sql
-- See fsm.create_machine for what the trailing deprecated parameters are for.
create or replace function fsm.create_state_machine_with_latest_statechart(
    statechart_name text   default null
  , shard_id        bigint default 1
  -- deprecated, removed in 0.3.0
  , shard_id_       bigint default null
  , named           text   default null
)
returns fsm.state_machine as
$$
    with desired_chart as (
        select c.id
        from fsm.get_latest_statechart(statechart_name => coalesce(statechart_name, named)) c
    )
    insert into fsm.state_machine (shard_id, statechart_id)
    select
        coalesce(shard_id_, create_state_machine_with_latest_statechart.shard_id) as shard_id,
        desired_chart.id as statechart_id
    from desired_chart
    -- stands in for the `strict` this function carried before the deprecated
    -- names forced defaults on. A null name yields no chart and so no row.
    where coalesce(shard_id_, create_state_machine_with_latest_statechart.shard_id) is not null
    returning *

$$ language sql volatile;

comment on function fsm.create_state_machine_with_latest_statechart(text, bigint, bigint, text) is $comment$
    Creates a new state machine but does not start it.
    You need to call fsm.start_machine after this
$comment$;

-- sql/function/get_finalized_parents.sql
-- See fsm.create_machine for what the trailing deprecated parameters are for.
-- shard_id and parent_path are qualified at every use because the tables this
-- reads have columns of both names, and in a SQL function the column wins.
create or replace function fsm.get_finalized_parents(
    machine_id  bigint default null
  , parent_path ltree  default null
  , shard_id    bigint default 1
  -- deprecated, removed in 0.3.0
  , shard                    bigint default null
  , active_state_parent_path ltree  default null
)
  returns setof fsm.state as
$$
  with all_parents as (
   select a_parent.*
   from fsm.state a_parent
   -- check that active_state_path is a child of a_parent
   where a_parent.node_path @> coalesce(get_finalized_parents.parent_path, active_state_parent_path)
     and exists (
        select 1
        from fsm.state_machine_state current_state
        where current_state.shard_id = coalesce(shard, get_finalized_parents.shard_id)
          and current_state.state_machine_id = machine_id
          and current_state.exited_at is null -- the state is currently active
          and current_state.statechart_id = a_parent.statechart_id
     )
  )
  select *
  from all_parents parent
  where
  -- A state can only be finalizable if it has no outgoing transitions. Here, we look
  -- at all the active children for the parent we are inspecting and make sure that
  -- all of them are at a point where the machine cannot move further inside that parent.
  not exists (
    select 1
    from fsm.state s
    join fsm.state_machine_state current_state
      on  current_state.shard_id = coalesce(shard, get_finalized_parents.shard_id)
      and current_state.state_machine_id = machine_id
      and current_state.state_id = s.id
      and current_state.exited_at is null -- the state is currently active
    where parent.id = s.parent_id
      and s.statechart_id = parent.statechart_id
      and exists (
        select 1 -- check that there is at least one outgoing transition for this state
        from fsm.transition t
        where t.statechart_id = s.statechart_id
          and t.source_state = s.id
      )
  )

  -- Parallel states are a special case. A parallel node is finalized when all its internal states
  -- have finilized already. Without this filter, we would end up considering parents with pending
  -- internal transitions.
  -- The key is to "dive-in" into the children of the parent we are inspecting and verify
  -- that only active states are marked as "is_final".
  -- The filter below is expressed in the negative: active non-final states should not exist.
  and not exists (
    select 1
    from fsm.state s -- the possible non-final, active state
    join fsm.state_machine_state current_state -- active parent state
      on  current_state.shard_id = coalesce(shard, get_finalized_parents.shard_id)
      and current_state.state_machine_id = machine_id
      and current_state.state_id = s.id
      and current_state.exited_at is null
    -- Dive into the children of the parent we are inspecting
    where parent.node_path @> s.parent_path -- check that the active parent state is actually a parent
      and parent.statechart_id = s.statechart_id
      -- get only the non "is_final" active states, since they determine wether or not there is
      -- more work to do for this sub-machine
      and not s.is_final -- check that the state under inspection is non-final
      -- Only non-final states can have children. Having an active parent state is fine, so
      -- we only need to make sure that this non-final state is a leaf in the tree. That is,
      -- it should not have any children.
      and not exists (
        select 1
        from fsm.state child
        where s.node_path @> child.parent_path
      )
  )
-- `strict` is gone because the deprecated names have to default to null to be
-- omittable. Nothing replaces it: a null argument makes every comparison below
-- null, so the function still returns no rows.
$$ language sql stable parallel safe;

comment on function fsm.get_finalized_parents (bigint, ltree, bigint, bigint, ltree) is
$comment$
A finalized parent can be defined as a compound state where all its children have reached
a state where no other transitions are possible.

We can determine this property by analyzing the possible transitions for internal active states
and whether or not these active states are final.

This function is used to generate "done.state.*" events for a state machine for all the parents
in an inheritance chain.
$comment$;

-- sql/function/get_initial_state.sql
-- Returns all the chains of nodes that are marked as initial. The chain
-- is interrupted whenever there is a child marked a non-initial
--
-- `statechart` is the name this took before 0.2.0, kept so that callers that
-- name their arguments keep working, and removed in 0.3.0. statechart_id is
-- qualified at every use because fsm.state has a column of that name, and in a
-- SQL function the column would quietly win.
create or replace function fsm.get_initial_state(
    statechart_id bigint default null
  -- deprecated, removed in 0.3.0
  , statechart    bigint default null
)
returns setof fsm.state as
$$
    with root as (
      select s.*
      from fsm.state s
      where s.statechart_id = coalesce(get_initial_state.statechart_id, get_initial_state.statechart)
        and s.is_initial
        and parent_id is null
    )
    select * from root

    union

    select child_s.*
    from fsm.state child_s
    where child_s.statechart_id = coalesce(get_initial_state.statechart_id, get_initial_state.statechart)
      and child_s.is_initial
      and child_s.parent_path <@ (select node_path from root)
      -- we make sure that the chaing of parents only contain initial nodes
      and not exists (
        select *
        from fsm.state parent
        -- <@ = left is descendant or equals to right
        where child_s.node_path <@ parent.node_path
          and not parent.is_initial
      )

$$ language sql stable parallel safe;

comment on function fsm.get_initial_state(bigint, bigint) is $comment$
    Get all initial states, when an initial state has children all of its
    initial states are also returned.
$comment$;

-- sql/function/handle_machine_events.sql
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
--
-- New in 0.2.0, so unlike the two argument form below it carries no deprecated
-- parameter names: nothing has ever called it by another spelling.
create or replace function fsm.handle_machine_events(
    machine_id bigint
  , events     fsm.machine_event[]
  , shard_id   bigint default 1
)
  returns void as
$$
  declare
    -- Resolved once. Several queries below read a shard_id column, so a bare
    -- shard_id would be ambiguous against the parameter of that name.
    machine_shard bigint := handle_machine_events.shard_id;
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
              on current_children_state.shard_id = machine_shard
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

        where current_state.shard_id = machine_shard
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
              update fsm.state_machine_state sms
              /* using clock_timestamp() instead of now(). now() uses
               * transaction time which can cause problem when another process
               * adds fsm.state_machine_state rows during the transaction. This
               * can cause the exited_at time to be earlier than the entered
               * time causing a constraint errors
               */
              set exited_at = clock_timestamp()
              where sms.shard_id = machine_shard
                and sms.state_machine_id = machine_id
                and sms.state_id in (select a.id from source_states a)
                -- if we are looping into the same state, we don't want to mark the current state as exited
                and sms.state_id not in (select a.id from target_states a)
                and sms.exited_at is null
            )

          select machine_shard, machine_id, machine_id, id, unnest(on_exit) as on_exit from source_states
        loop
          execute format('select %I.%I(%L::fsm_event_payload)',
              coalesce((on_exit_function_call.on_exit).schema_name, 'public')
            , (on_exit_function_call.on_exit).function_name
            , ( machine_shard
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
                  machine_shard as shard_id
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

          select machine_shard, machine_id, machine_id, id, unnest(on_entry) as on_entry from target_states
        loop
          execute format('select %I.%I(%L::fsm_event_payload)',
              coalesce((on_entry_function_call.on_entry).schema_name, 'public')
            , (on_entry_function_call.on_entry).function_name
            , ( machine_shard
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
            ( machine_shard
            , machine_id
            , format('done.state.%s', (state_transition.target_state).id)
            , handled.data
            );

          insert into fsm.state_machine_event (shard_id, state_machine_id, name, data)
            select machine_shard as shard_id, machine_id as state_machine_id
                 , format('done.state.%s', f.id)
                 , handled.data as data
            from fsm.get_finalized_parents(
                machine_id  => handle_machine_events.machine_id
              , parent_path => (state_transition.target_state).parent_path
              , shard_id    => machine_shard
            ) f;
        end if;
        -- event-generation}}}

      end loop;

    end loop;

    return;
  end;
$$ language plpgsql volatile strict;

-- The queue-reading form, and the one 0.1.0 shipped. It looks up whatever is
-- still unhandled for the machine, marks it, and hands it to the function
-- above. See fsm.create_machine for what the trailing deprecated parameter is
-- for.
--
-- Events sent with fsm.notify_state_machine are already handled by the time it
-- returns, because the insert fires notify_machine_event. This form is for
-- events inserted with that trigger disabled, which have to set handled_at to
-- null explicitly now that the column defaults to now().
create or replace function fsm.handle_machine_events(
    machine_id bigint default null
  , shard_id   bigint default 1
  -- deprecated, removed in 0.3.0
  , shard      bigint default null
)
  returns void as
$$
  declare
    machine_shard bigint := coalesce(shard, handle_machine_events.shard_id);
    events_ fsm.machine_event[];
  begin
    with unhandled as (
        select *
        from fsm.state_machine_event e
        where e.shard_id = machine_shard
          and e.state_machine_id = machine_id
          and e.handled_at is null
      )
    , handling as (
        update fsm.state_machine_event ev set handled_at = now()
        where (ev.shard_id, ev.id) in (select u.shard_id, u.id from unhandled u)
      )
    select array_agg(
        (u.shard_id, u.id, u.state_machine_id, u.created_at, u.name, u.data)::fsm.machine_event
        order by u.created_at, u.id
    )
    into events_
    from unhandled u;

    if events_ is not null then
      perform fsm.handle_machine_events(
          machine_id => machine_id
        , events     => events_
        , shard_id   => machine_shard);
    end if;

    return;
  end;
-- `strict` is gone because the deprecated name has to default to null to be
-- omittable. Nothing replaces it: a null argument matches no events, so the
-- function still does nothing.
$$ language plpgsql volatile;

-- sql/function/is_state_active.sql
-- See fsm.create_machine for what the trailing deprecated parameters are for.
-- shard_id and state_id are qualified at every use because
-- fsm.state_machine_state has columns of both names.
create or replace function fsm.is_state_active(
    machine_id bigint default null
  , state_id   text   default null
  , shard_id   bigint default 1
  -- deprecated, removed in 0.3.0
  , shid       bigint default null
  , smid       bigint default null
  , state      text   default null
)
  returns bool as
$$
  begin

    -- stands in for the `strict` this function carried before the deprecated
    -- names forced defaults on
    if coalesce(is_state_active.machine_id, smid) is null
       or coalesce(is_state_active.state_id, state) is null
       or coalesce(shid, is_state_active.shard_id) is null
    then
      return null;
    end if;

    perform true
    from fsm.state_machine_state sms
    where sms.state_machine_id = coalesce(is_state_active.machine_id, smid)
      and sms.shard_id = coalesce(shid, is_state_active.shard_id)
      and sms.state_id = coalesce(is_state_active.state_id, state)
      and sms.exited_at is null; -- the state is currently active

    if not found then
      return false;
    else
      return true;
    end if;
  end;
$$ language plpgsql stable;

-- sql/function/is_valid_transition.sql
-- See fsm.create_machine for what the trailing deprecated parameters are for.
create or replace function fsm.is_valid_transition(
    machine_id bigint default null
  , event_name text   default null
  , shard_id   bigint default 1
  -- deprecated, removed in 0.3.0
  , shard      bigint default null
  , event_     text   default null
)
  returns bool as
$$
  begin
      -- stands in for the `strict` this function carried before the deprecated
      -- names forced defaults on
      if is_valid_transition.machine_id is null
         or coalesce(event_name, event_) is null
         or coalesce(shard, is_valid_transition.shard_id) is null
      then
        return null;
      end if;

      perform true
      from fsm.state_machine_state current_state

      join fsm.state child_state
          on  child_state.statechart_id = current_state.statechart_id
          and child_state.id = current_state.state_id

      join fsm.state state
          on  state.statechart_id = current_state.statechart_id
          -- is state.node_path an ancestor of current_state.node_path (or equal)?
          -- this is used to get all of the current_state parents, including the current_state
          and state.node_path @> child_state.node_path

      -- check if the transition named by event_name is a valid one for
      -- the current_state. That is, we need to see if "current_state"
      -- appears as a "source_state" in the transition table and has
      -- an associated "event" name equal to it
      join fsm.transition transition
          on transition.statechart_id = state.statechart_id
          and transition.source_state = state.id
          and transition.event = coalesce(event_name, event_)

      where   current_state.shard_id = coalesce(shard, is_valid_transition.shard_id)
          and current_state.state_machine_id = is_valid_transition.machine_id
          and current_state.exited_at is null;

      if not found then
          return false;
      end if;

    return true;
  end;
$$ language plpgsql stable;

-- sql/function/notify_state_machine.sql
-- See fsm.create_machine for what the trailing deprecated parameters are for.
-- This function was never strict, so a null id still reaches the insert and is
-- rejected by the column's not null constraint, as it was before 0.2.0.
create or replace function fsm.notify_state_machine(
    machine_id bigint default null
  , event_name text   default null
  , data       jsonb  default '{}'
  , shard_id   bigint default 1
  -- deprecated, removed in 0.3.0
  , shard      bigint default null
  , machine    bigint default null
  , event      text   default null
)
    returns void as
$$
    insert into fsm.state_machine_event
        (shard_id, state_machine_id, name, data) values
        ( coalesce(shard, notify_state_machine.shard_id)
        , coalesce(notify_state_machine.machine_id, machine)
        , coalesce(event_name, event)
        , notify_state_machine.data
        )
$$ language sql volatile security definer;

comment on function fsm.notify_state_machine is 'send an event to a specific state machine';

-- sql/function/start_machine.sql
-- See fsm.create_machine for what the trailing deprecated parameter is for.
-- This function was never strict, so a machine that does not exist still
-- returns null rather than raising, as it did before 0.2.0.
create or replace function fsm.start_machine(
    machine_id   bigint default null
  , initial_data jsonb  default '{}'
  , shard_id     bigint default 1
  -- deprecated, removed in 0.3.0
  , shard        bigint default null
)
  returns fsm.state_machine as
  $$
    declare
      -- Resolved once. Every query below reads a shard_id column, so a bare
      -- shard_id would be ambiguous against the parameter of that name.
      machine_shard bigint := coalesce(shard, start_machine.shard_id);
      initial fsm.state%rowtype;
      machine fsm.state_machine%rowtype;
      error_message text;
      error_state text;
      error_context text;
      error_detail text;
      on_entry_ fsm_callback_name;
    begin

      select * into machine from fsm.state_machine sm where sm.shard_id = machine_shard and sm.id = start_machine.machine_id;

      for initial in select * from fsm.get_initial_state(statechart_id => machine.statechart_id)
      loop

        insert into fsm.state_machine_state
          (shard_id, state_machine_id, statechart_id, state_id) values
          (machine_shard, machine.id, machine.statechart_id, initial.id);

        foreach on_entry_ in array initial.on_entry
        loop
            -- This call will look like something like this
            -- select my_schema.my_function(
            --  (1, 199, '__initial__', '{...}', null, 233, 'on_entry')::fsm.event_payload
            -- )
            execute format('select %I.%I(%L::fsm_event_payload)',
                coalesce((on_entry_).schema_name::text, 'public')
              , (on_entry_).function_name
              , ( machine_shard
                , machine.id
                , '__initial__'
                , initial_data
                , null
                , initial.id
                , 'on_entry'
                )::fsm_event_payload
            );
        end loop;

      end loop;

      return machine;

    exception when others then
      get stacked diagnostics
        error_message = message_text,
        error_context = pg_exception_context,
        error_detail = pg_exception_detail,
        error_state = returned_sqlstate;
      raise exception
        using errcode=error_state,
          message=error_message,
          detail = error_detail,
          hint=format('Error while handling state machine event: %s', error_context);
    end
  $$ language plpgsql volatile;

comment on function fsm.start_machine(bigint, jsonb, bigint, bigint) is $comment$
    Starts a given state machine by activating all of its initial states and
    executing the corresponding on_entry callbacks.
$comment$;

-- sql/function/start_machine_with_latest_statechart.sql
-- shard_id already had this name before 0.2.0, so it needs no deprecated
-- alias; `named` does. shard_id is qualified at every use because the rows
-- this selects from have a column of that name.
create or replace function fsm.start_machine_with_latest_statechart(
    statechart_name text   default null
  , initial_data    jsonb  default '{}'
  , shard_id        bigint default 1
  -- deprecated, removed in 0.3.0
  , named           text   default null
)
returns setof fsm.state_machine as
$$
    select m.*
    from fsm.create_state_machine_with_latest_statechart(
             statechart_name => coalesce(start_machine_with_latest_statechart.statechart_name, named)
           , shard_id        => start_machine_with_latest_statechart.shard_id
         ) m
    join fsm.start_machine(
             machine_id   => m.id
           , initial_data => start_machine_with_latest_statechart.initial_data
           , shard_id     => start_machine_with_latest_statechart.shard_id
         ) on true

    -- create_state_machine_with_latest_statechart returns a null row for a
    -- name that matches no chart, where this function used to return no rows
    -- at all because it was strict
    where m.id is not null

    limit 1
$$ language sql volatile
    rows 1;

comment on function fsm.start_machine_with_latest_statechart(text, jsonb, bigint, text) is $comment$
    Creates and starts a state machine with the latest statechart with the name passed.
$comment$;

-- sql/trigger/notify_machine_event.sql, the function only: the trigger that
-- uses it is unchanged.
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
          machine_id => machine_.state_machine_id
        , events     => machine_.events
        , shard_id   => machine_.shard_id
      );
    end loop;

    return null;
  end;
$$ language plpgsql;

comment on function fsm.trig_notify_machine_event is $comment$
  Handles all of the newly inserted events for all affected state machines.
$comment$;

\echo Use "ALTER EXTENSION pg_statecharts UPDATE" to load this file. \quit

-- Upgrade from 0.2.0 to 0.2.1.
--
-- Concurrent events for one machine fail loudly instead of diverging.
--
-- fsm.handle_machine_events reads a machine's active states and then exits and
-- enters states based on what it read, with nothing holding those two
-- together. Under read committed, PostgreSQL's default, two transactions
-- sending events to the same machine at once both read the configuration as it
-- was before either committed, and both acted on it: a machine with one
-- transition to a final `paid` and another to a final `voided` could end up in
-- `paid` and `voided` at the same time, with both on_entry callbacks run.
--
-- Exiting the source states now checks that every state the transition named
-- was still active, and raises 40001 when one of them was not. 40001 rather
-- than a code of its own because the README already asks callers to send
-- events at repeatable read or serializable and to retry serialization
-- failures; this needs no new handling from a client that does.
--
-- It is a backstop and not a substitute for the isolation level. It catches
-- the way this goes wrong in practice -- the losing transaction's source
-- states exited underneath it -- but it is not a proof that nothing else can.
--
-- fsm.trig_notify_machine_event also walks the machines of a multi-machine
-- insert in a deterministic order now, so that two such statements cannot
-- write fsm.state_machine_state for the same machines in opposite orders and
-- deadlock on each other.
--
-- Only function bodies change. No signature, table, type or trigger is
-- touched, so there is nothing to drop and nothing a database mid-flight
-- through this upgrade can be caught between.
--
-- 0.2.1 is not released. Versions are bumped per change rather than at release
-- time, so the next change to the extension gets a 0.2.2 script of its own
-- next to this one, and so on; 0.3.0 is reserved for the release itself, which
-- is where the parameter names marked "deprecated, removed in 0.3.0" go. The
-- .deb examples in the READMEs stay at the newest tagged version until then.

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
    -- How many states the transition below says to exit, and how many the
    -- update actually found still active. See the check that compares them.
    expected_exits bigint;
    actual_exits bigint;
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
        -- Mark the source states exited. This used to be a CTE of the loop
        -- below; it is a statement of its own because how many rows it
        -- updates is what the check after it reads.
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

        , expected as (
            -- Every state in source_states was active when the transition
            -- query read it: the source is the row that query matched on
            -- exited_at is null, and the children came from a join filtered
            -- the same way. So each one that is not also a target has to
            -- still be active now, and the update has to find all of them.
            select count(distinct a.id) as n
            from source_states a
            -- if we are looping into the same state, we don't want to mark the current state as exited
            where a.id not in (select x.id from target_states x)
          )

        , updated as (
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
              and sms.state_id not in (select a.id from target_states a)
              and sms.exited_at is null
            returning 1
          )

        select (select n from expected), (select count(*) from updated)
        into expected_exits, actual_exits;

        -- This is what makes a concurrent divergence loud rather than silent.
        --
        -- Handling reads the machine's active states and then acts on what it
        -- read, and nothing holds those two together. Under read committed,
        -- PostgreSQL's default, two transactions sending events to the same
        -- machine at once both read the configuration as it was before either
        -- committed. Without this check the one that loses the race applies a
        -- transition the machine has already moved past: a machine with one
        -- transition to a final `paid` and another to a final `voided` ends up
        -- in `paid` and `voided` together, both final, with both on_entry
        -- callbacks run. With it, the loser finds the states it meant to exit
        -- already exited, and raises.
        --
        -- A backstop, not a substitute for the isolation level. It catches the
        -- way this goes wrong in practice -- the losing transaction's source
        -- states exited underneath it -- but it is not a proof that nothing
        -- else can. Send events at repeatable read or serializable; see the
        -- README.
        --
        -- Raised as 40001 rather than as a code of its own so that a client
        -- which already retries serialization failures -- which the README
        -- asks for, and which repeatable read and serializable need from it
        -- anyway -- recovers from this without being taught anything new.
        if actual_exits <> expected_exits then
          raise exception
              'expected to exit % state(s) of machine % in shard % handling %, but only % of them were still active'
            , expected_exits
            , handle_machine_events.machine_id
            , machine_shard
            , handled.name
            , actual_exits
            using errcode = '40001'
                , detail  = 'Another transaction exited them in between this one'
                            ' reading the machine''s active states and acting on'
                            ' what it read.'
                , hint    = 'Retry the transaction.';
        end if;

        for on_exit_function_call in
          with
            source_states as (
              select *
              from
                unnest(state_transition.children_source_states || state_transition.source_state)
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
    -- The same check, and deliberately the same message, as fsm.start_machine.
    -- Draining the queue of a machine that does not exist looks exactly like
    -- the ordinary case of a machine with nothing pending, so without this it
    -- is the one remaining way to address a missing machine and be told
    -- nothing at all.
    --
    -- The form above does not carry it. The trigger only reaches that one with
    -- machines the event table's foreign key has already vouched for, and the
    -- lookup would be paid on every event rather than on this rarely used
    -- path.
    if handle_machine_events.machine_id is not null
       and not exists (
         select 1
         from fsm.state_machine sm
         where sm.shard_id = machine_shard
           and sm.id = handle_machine_events.machine_id
       )
    then
      raise exception 'state machine with id % and shard_id % does not exist'
          , handle_machine_events.machine_id, machine_shard
        using detail = 'The id and the shard_id together identify a machine,'
                       ' so a machine created under a different shard_id is'
                       ' not found by this one. Pass the shard_id the machine'
                       ' was created with, or omit the argument everywhere so'
                       ' that every call uses the default of 1.';
    end if;

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
        -- Deterministic on purpose. Each iteration writes its machine's rows
        -- in fsm.state_machine_state: exited_at stamped on the states it
        -- leaves, a row inserted for each state it enters. Nothing here takes
        -- a lock of its own, but both of those block a second transaction
        -- doing the same to that machine until this one commits -- an updated
        -- row holds its lock, and idx_state makes a duplicate active state
        -- wait on whoever inserted first. So two statements inserting events
        -- for the same machines in different orders would be able to block
        -- each other in opposite orders and deadlock. Ordering here settles
        -- it for a single statement; callbacks that send events on to further
        -- machines still pick their own order, and a deadlock between those
        -- is detected and reported as 40P01. Within one machine the events
        -- are already ordered by the array_agg above.
        order by inserted.shard_id, inserted.state_machine_id
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

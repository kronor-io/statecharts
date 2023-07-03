-- Deploy statecharts:function/handle_machine_events to pg

BEGIN;

  create or replace function fsm.handle_machine_events(shard bigint, machine_id bigint)
    returns void as
  $$
    declare
      handled fsm.state_machine_event%rowtype;
      state_transition record;
      on_exit_function_call record;
      on_entry_function_call record;
    begin

      -- First mark all pending events as handled and then select the set of source
      -- active states and future target states based on the event transition. This
      -- selection will be used to perform the transition effects and record the new
      -- machine current states.

      -- regardless of whether or not there exists a valid transition for the
      -- unhandled events, we need to mark them as handled. This is why we start
      -- the process with the update.

      -- The reason for using a select and then an update, instead of just issuing a plain
      -- update with conditions is that we want access to the list of events in creation order
      -- rather than the order that postgres found most convenient.
      for handled in
          with unhandled as (
            select *
            from fsm.state_machine_event
            where shard_id = shard
              and state_machine_id = machine_id
              and handled_at is null
            order by created_at, id asc
          )
          , handling as (
            update fsm.state_machine_event set handled_at = now()
            where (shard_id, id) in (select shard_id, id from unhandled)
          )
          select * from unhandled
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
              , '{}'
              );

            insert into fsm.state_machine_event (shard_id, state_machine_id, name, data)
              select shard as shard_id, machine_id as state_machine_id
                   , format('done.state.%s', f.id)
                   , '{}' as data
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

COMMIT;

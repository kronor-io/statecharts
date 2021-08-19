-- Deploy statecharts:function/is_valid_transition to pg

BEGIN;

  create or replace function fsm.is_valid_transition(shard bigint, machine_id bigint, event_ text)
    returns bool as
  $$
    begin
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

        -- check if the transition named "event_" is a valid one for
        -- the current_state. That is, we need to see if "current_state"
        -- appears as a "source_state" in the transition table and has
        -- an associated "event" name equals to event_
        join fsm.transition transition
            on transition.statechart_id = state.statechart_id
            and transition.source_state = state.id
            and transition.event = event_

        where   current_state.shard_id = shard
            and current_state.state_machine_id = machine_id
            and current_state.exited_at is null;

        if not found then
            return false;
        end if;

      return true;
    end;
  $$ language plpgsql stable strict;


COMMIT;

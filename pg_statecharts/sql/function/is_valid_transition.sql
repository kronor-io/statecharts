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

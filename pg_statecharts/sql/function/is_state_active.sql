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

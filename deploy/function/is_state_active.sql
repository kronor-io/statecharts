-- Deploy statecharts:function/is_state_active to pg

BEGIN;

  create or replace function fsm.is_state_active(shid bigint, smid bigint, state text)
    returns bool as
  $$
    begin

      perform true
      from fsm.state_machine_state 
      where state_machine_id = smid 
        and shard_id = shid
        and state_id = state;

      if not found then
        return false;
      else
        return true;
      end if;
    end;
  $$ language plpgsql stable strict;


COMMIT;

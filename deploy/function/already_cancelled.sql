-- Deploy statecharts:function/already_cancelled to pg

BEGIN;

  create or replace function fsm.already_cancelled(smid bigint)
    returns bool as
  $$
    begin
      perform true
      from fsm.state_machine_state where state_machine_id = smid and state_id = 'cancelled'; -- and exited_at = null; should we have this?
      if not found then
        return false;
      else
        return true;
      end if;
    end;
  $$ language plpgsql stable strict;


COMMIT;

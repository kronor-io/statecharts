-- Revert statecharts:trigger/set_state_parent_path from pg

BEGIN;

  drop trigger if exists set_state_parent_path on fsm.state;
  drop function if exists fsm.trig_set_state_parent_path;

COMMIT;

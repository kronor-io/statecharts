-- Revert statecharts:trigger/check_valid_initial_state from pg

BEGIN;

  drop trigger if exists check_valid_initial_state_insert on fsm.state;
  drop trigger if exists check_valid_initial_state_update on fsm.state;
  drop trigger if exists check_valid_initial_state_delete on fsm.state;

  drop function if exists fsm.trig_check_valid_initial_state;

COMMIT;

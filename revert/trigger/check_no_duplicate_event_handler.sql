-- Revert statecharts:trigger/check_no_duplicate_event_handler from pg

BEGIN;

  drop trigger if exists check_no_duplicate_event_handler on fsm.transition;
  drop function if exists fsm.trig_check_no_duplicate_event_handler;

COMMIT;

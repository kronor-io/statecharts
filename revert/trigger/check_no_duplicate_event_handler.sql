-- Revert statecharts:trigger/check_no_duplicate_event_handler from pg

BEGIN;

  drop trigger if exists check_no_duplicate_event_handler_insert on fsm.transition;
  drop trigger if exists check_no_duplicate_event_handler_update on fsm.transition;
  drop function if exists fsm.trig_check_no_duplicate_event_handler;

COMMIT;

-- Revert statecharts:trigger/notify_machine_event from pg

BEGIN;

  drop trigger if exists notify_machine_event on fsm.state_machine_event;
  drop function if exists fsm.trig_notify_machine_event;

COMMIT;

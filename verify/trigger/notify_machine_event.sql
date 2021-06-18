-- Verify statecharts:trigger/notify_machine_event on pg

BEGIN;

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'notify_machine_event'
    and event_object_schema = 'fsm'
    and event_object_table = 'state_machine_event';

ROLLBACK;

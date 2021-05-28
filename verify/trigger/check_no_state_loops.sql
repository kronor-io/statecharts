-- Verify statecharts:trigger/check_no_state_loops on pg

BEGIN;

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_no_state_loops'
    and event_object_schema = 'fsm'
    and event_object_table = 'state';

ROLLBACK;

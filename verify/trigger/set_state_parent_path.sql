-- Verify statecharts:trigger/set_state_parent_path on pg

BEGIN;

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'set_state_parent_path'
    and event_object_schema = 'fsm'
    and event_object_table = 'state';

ROLLBACK;

-- Verify statecharts:trigger/check_no_duplicate_event_handler on pg

BEGIN;

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_no_duplicate_event_handler_insert'
    and event_object_schema = 'fsm'
    and event_object_table = 'transition';

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_no_duplicate_event_handler_update'
    and event_object_schema = 'fsm'
    and event_object_table = 'transition';

ROLLBACK;

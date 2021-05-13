-- Verify statecharts:trigger/check_valid_initial_state on pg

BEGIN;


  select has_function_privilege('fsm.get_initial_state(bigint)', 'execute');

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_valid_initial_state_insert'
    and event_object_schema = 'fsm'
    and event_object_table = 'state';

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_valid_initial_state_update'
    and event_object_schema = 'fsm'
    and event_object_table = 'state';

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_valid_initial_state_delete'
    and event_object_schema = 'fsm'
    and event_object_table = 'state';

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_valid_initial_state_insert'
    and event_object_schema = 'fsm'
    and event_object_table = 'compound_state';

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_valid_initial_state_update'
    and event_object_schema = 'fsm'
    and event_object_table = 'compound_state';

ROLLBACK;

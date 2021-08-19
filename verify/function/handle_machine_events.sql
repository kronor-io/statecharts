-- Verify statecharts:function/handle_machine_events on pg

BEGIN;

  select  has_function_privilege('fsm.handle_machine_events(bigint, bigint)', 'execute');

ROLLBACK;

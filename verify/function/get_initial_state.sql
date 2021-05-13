-- Verify statecharts:function/get_initial_state on pg

BEGIN;

  select  has_function_privilege('fsm.get_initial_state(bigint)', 'execute');

ROLLBACK;

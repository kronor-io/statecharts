-- Verify statecharts:function/create_state_machine_with_latest_statechart on pg

BEGIN;

  select  has_function_privilege('fsm.create_state_machine_with_latest_statechart(bigint, text)', 'execute');

ROLLBACK;

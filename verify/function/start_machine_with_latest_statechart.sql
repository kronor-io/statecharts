-- Verify statecharts:function/start_machine_with_latest_statechart on pg

BEGIN;

  select  has_function_privilege('fsm.start_machine_with_latest_statechart(bigint, text)', 'execute');

ROLLBACK;

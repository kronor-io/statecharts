-- Verify statecharts:function/start_machine on pg

BEGIN;

  select  has_function_privilege('fsm.start_machine(bigint, bigint, bigint)', 'execute');

ROLLBACK;

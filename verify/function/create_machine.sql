-- Verify statecharts:function/create_machine on pg

BEGIN;

  select  has_function_privilege('fsm.create_machine(bigint, bigint)', 'execute');

ROLLBACK;

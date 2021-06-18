-- Verify statecharts:function/get_finalized_parents on pg

BEGIN;

  select  has_function_privilege('fsm.get_finalized_parents(bigint, bigint, ltree)', 'execute');

ROLLBACK;

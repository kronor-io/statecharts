-- Verify statecharts:function/is_valid_transition on pg

BEGIN;

  select  has_function_privilege('fsm.is_valid_transition(bigint, bigint, text)', 'execute');

ROLLBACK;

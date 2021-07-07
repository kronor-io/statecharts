-- Verify statecharts:function/notify_state_machine on pg

BEGIN;

  select has_function_privilege('fsm.notify_state_machine(bigint, bigint, text, jsonb)', 'execute');

ROLLBACK;

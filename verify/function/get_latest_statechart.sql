-- Verify statecharts:function/get_latest_statechart on pg

BEGIN;


  select has_function_privilege('fsm.get_latest_statechart(text)', 'execute');

ROLLBACK;

-- Revert statecharts:function/get_initial_state from pg

BEGIN;

  drop function if exists fsm.get_initial_state;

COMMIT;

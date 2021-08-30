-- Revert statecharts:function/is_state_active from pg

BEGIN;

    drop function if exists fsm.is_state_active;

COMMIT;

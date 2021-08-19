-- Revert statecharts:function/is_valid_transition from pg

BEGIN;

    drop function if exists fsm.is_valid_transition;

COMMIT;

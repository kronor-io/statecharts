-- Revert statecharts:function/already_cancelled from pg

BEGIN;

    drop function if exists fsm.already_cancelled;

COMMIT;

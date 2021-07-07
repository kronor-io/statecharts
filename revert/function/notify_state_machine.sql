-- Revert statecharts:function/notify_state_machine from pg

BEGIN;

    drop function if exists fsm.notify_state_machine;

COMMIT;

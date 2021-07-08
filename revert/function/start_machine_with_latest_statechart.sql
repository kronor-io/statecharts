-- Revert statecharts:function/start_machine_with_latest_statechart from pg

BEGIN;

    drop function if exists fsm.start_machine_with_latest_statechart(bigint, text, jsonb);

COMMIT;

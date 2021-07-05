-- Revert statecharts:function/get_latest_statechart from pg

BEGIN;

    drop function if exists fsm.get_latest_statechart(text);

COMMIT;

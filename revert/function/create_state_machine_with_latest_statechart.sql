-- Revert statecharts:function/create_state_machine_with_latest_statechart from pg

BEGIN;

-- XXX Add DDLs here.
    drop function if exists fsm.create_state_machine_with_latest_statechart;

COMMIT;

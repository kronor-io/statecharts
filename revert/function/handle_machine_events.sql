-- Revert statecharts:function/handle_machine_events from pg

BEGIN;

drop function if exists fsm.handle_machine_events;

COMMIT;

-- Revert statecharts:type/fsm_event_payload from pg

BEGIN;

    drop function if exists fsm.handle_machine_events;
    drop type if exists fsm_event_payload;

COMMIT;

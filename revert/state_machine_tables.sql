-- Revert statecharts:state_machine_tables from pg

BEGIN;

  DROP TABLE IF EXISTS fsm.state_machine_event;
  DROP TABLE IF EXISTS fsm.state_machine_state;
  DROP TABLE IF EXISTS fsm.state_machine;

COMMIT;

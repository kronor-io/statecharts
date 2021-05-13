-- Revert statecharts:fsm_schema from pg

BEGIN;

  DROP SCHEMA fsm;

COMMIT;

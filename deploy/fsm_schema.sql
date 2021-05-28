-- Deploy statecharts:fsm_schema to pg

BEGIN;

  CREATE SCHEMA fsm;

  SET client_min_messages TO warning;
  CREATE EXTENSION IF NOT EXISTS ltree;
  RESET client_min_messages;

COMMIT;

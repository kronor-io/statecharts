-- Deploy statecharts:fsm_schema to pg

BEGIN;

  CREATE SCHEMA fsm;

  SET client_min_messages TO warning;
  CREATE EXTENSION IF NOT EXISTS ltree;
  CREATE EXTENSION IF NOT EXISTS semver;
  RESET client_min_messages;

  comment on schema fsm is 'Schema containing the statecharts implementation.';

COMMIT;

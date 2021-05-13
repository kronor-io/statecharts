-- Verify statecharts:fsm_schema on pg

BEGIN;

SELECT pg_catalog.has_schema_privilege('fsm', 'usage');

ROLLBACK;

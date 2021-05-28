-- Revert statecharts:statechart_tables from pg

BEGIN;

DROP TABLE fsm.transition;
DROP TYPE transition_scope;
DROP TABLE fsm.state;
DROP TABLE fsm.statechart;

COMMIT;

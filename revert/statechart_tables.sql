-- Revert statecharts:statechart_tables from pg

BEGIN;

DROP TABLE fsm.transition;
DROP TABLE fsm.state;
DROP TABLE fsm.statechart;
DROP TYPE fsm_callback_name;

COMMIT;

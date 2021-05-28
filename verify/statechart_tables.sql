-- Verify statecharts:statechart_tables on pg
\unset ECHO
\set QUIET 1
\pset pager off

BEGIN;

  select * from fsm.statechart;
  select * from fsm.state;
  select * from fsm.transition;

ROLLBACK;

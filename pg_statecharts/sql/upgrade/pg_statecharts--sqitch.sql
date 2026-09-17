-- Adopts a schema deployed with sqitch into the extension.
--
--     create extension pg_statecharts version 'sqitch';
--     alter extension pg_statecharts update;
--
-- Before there was an extension, this repository was deployed by running
-- sqitch against the deploy/ directory. A database set up that way already
-- has every object in the fsm schema, so CREATE EXTENSION cannot create them
-- again. This script creates nothing: it only tells PostgreSQL that the
-- objects sqitch made now belong to the extension, which is what
-- ALTER EXTENSION ... ADD is for.
--
-- The result is the exact schema 0.0.0 installed -- 0.0.0 was generated from
-- deploy/ -- so the second statement above continues along the 0.0.0 upgrade
-- path, which converts the version column off the semver extension and drops
-- it. Nothing here touches data.
--
-- Triggers, indexes, constraints and comments belong to their tables and come
-- along with them. Only tables, sequences, types, functions and the schema
-- have to be listed.
--
-- After this, sqitch must not be used to revert any of the fsm changes in the
-- plan. Some of the revert scripts are CREATE OR REPLACE FUNCTION and would
-- quietly swap an older body into a function the extension now owns; the rest
-- fail because the extension protects its objects. See the README.

\echo Use "CREATE EXTENSION pg_statecharts VERSION 'sqitch'" to load this file. \quit

alter extension pg_statecharts add schema fsm;

alter extension pg_statecharts add type fsm_callback_name;
alter extension pg_statecharts add type fsm_event_payload;

alter extension pg_statecharts add table fsm.statechart;
alter extension pg_statecharts add sequence fsm.statechart_id_seq;
alter extension pg_statecharts add table fsm.state;
alter extension pg_statecharts add table fsm.transition;
alter extension pg_statecharts add table fsm.state_machine;
alter extension pg_statecharts add sequence fsm.state_machine_id_seq;
alter extension pg_statecharts add table fsm.state_machine_state;
alter extension pg_statecharts add table fsm.state_machine_event;
alter extension pg_statecharts add sequence fsm.state_machine_event_id_seq;

alter extension pg_statecharts add function fsm.create_machine(bigint, bigint);
alter extension pg_statecharts add function fsm.create_state_machine_with_latest_statechart(bigint, text);
alter extension pg_statecharts add function fsm.get_finalized_parents(bigint, bigint, ltree);
alter extension pg_statecharts add function fsm.get_initial_state(bigint);
alter extension pg_statecharts add function fsm.get_latest_statechart(text);
alter extension pg_statecharts add function fsm.handle_machine_events(bigint, bigint);
alter extension pg_statecharts add function fsm.is_state_active(bigint, bigint, text);
alter extension pg_statecharts add function fsm.is_valid_transition(bigint, bigint, text);
alter extension pg_statecharts add function fsm.notify_state_machine(bigint, bigint, text, jsonb);
alter extension pg_statecharts add function fsm.start_machine(bigint, bigint, jsonb);
alter extension pg_statecharts add function fsm.start_machine_with_latest_statechart(bigint, text, jsonb);

alter extension pg_statecharts add function fsm.trig_check_no_cross_boundary_transition();
alter extension pg_statecharts add function fsm.trig_check_no_duplicate_event_handler();
alter extension pg_statecharts add function fsm.trig_check_no_state_loops();
alter extension pg_statecharts add function fsm.trig_check_no_transition_for_final();
alter extension pg_statecharts add function fsm.trig_check_no_final_state_with_transition();
alter extension pg_statecharts add function fsm.trig_check_valid_initial_state();
alter extension pg_statecharts add function fsm.trig_notify_machine_event();
alter extension pg_statecharts add function fsm.trig_set_state_parent_path();

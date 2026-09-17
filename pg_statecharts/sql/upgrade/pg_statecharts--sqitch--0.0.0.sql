-- From a schema adopted out of a sqitch deployment to 0.0.0.
--
-- There is nothing to do. 0.0.0 was generated from the deploy/ directory, so
-- an adopted schema already is a 0.0.0 schema, minus two C functions that the
-- 0.0.0 -> 0.1.0 script drops with IF EXISTS. This file exists so that
--
--     alter extension pg_statecharts update;
--
-- can find a path from 'sqitch' to the current version through the 0.0.0
-- upgrade script, instead of that script being duplicated here.

\echo Use "ALTER EXTENSION pg_statecharts UPDATE" to load this file. \quit

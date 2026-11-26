\echo Use "ALTER EXTENSION pg_statecharts_dev UPDATE" to load this file. \quit

-- Upgrade from 0.2.0 to 0.2.1.
--
-- Nothing in the dev tooling changes here. The script exists because the two
-- extensions are versioned and released together -- scripts/package_release.sh
-- refuses to build when their versions disagree -- so a database that has this
-- one installed still needs a path from 0.2.0 to 0.2.1 to follow.
--
-- What changed is fsm.handle_machine_events refusing to apply a transition
-- whose source states another transaction has already exited; see
-- pg_statecharts--0.2.0--0.2.1.sql.

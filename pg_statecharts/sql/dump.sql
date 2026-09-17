-- pg_dump leaves out the tables of an extension unless the extension registers
-- them as configuration tables. Without this, a backup of a database that uses
-- pg_statecharts holds every application row with its state_machine_id and
-- none of the machines those ids point at: after a restore the fsm tables are
-- empty, the identity sequences start over from 1, and the first new machines
-- collide with the ids the application tables still carry. 0.0.0 never
-- registered its tables, so backups taken under it do not contain the fsm
-- data; see "Backups" in the README.
--
-- The sequences are listed too, so that a restored database carries on
-- numbering where the dumped one left off. pg_dump orders the data by the
-- foreign keys between these tables, and the triggers on fsm.state and
-- fsm.transition qualify their ltree references so that they work under the
-- empty search_path that pg_restore loads data with.
select pg_catalog.pg_extension_config_dump('fsm.statechart', '');
select pg_catalog.pg_extension_config_dump('fsm.state', '');
select pg_catalog.pg_extension_config_dump('fsm.transition', '');
select pg_catalog.pg_extension_config_dump('fsm.state_machine', '');
select pg_catalog.pg_extension_config_dump('fsm.state_machine_state', '');
select pg_catalog.pg_extension_config_dump('fsm.state_machine_event', '');
select pg_catalog.pg_extension_config_dump('fsm.statechart_id_seq', '');
select pg_catalog.pg_extension_config_dump('fsm.state_machine_id_seq', '');
select pg_catalog.pg_extension_config_dump('fsm.state_machine_event_id_seq', '');

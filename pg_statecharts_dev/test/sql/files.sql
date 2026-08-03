-- Reading and writing files on the database host.
--
-- Everything is written under /tmp because that is the one directory the tests
-- can rely on already existing: SQL has no way to create one.

-- Error CONTEXT carries plpgsql line numbers, which would make this
-- expected output break on every unrelated edit.
\set SHOW_CONTEXT never
create extension if not exists pg_statecharts_dev cascade;

\set dir '/tmp/pg_statecharts_test'
\set chart_a '/tmp/pg_statecharts_test_a-1.0.scxml'
\set chart_b '/tmp/pg_statecharts_test_b-1.0.scxml'

--
-- Writing files
--

-- Content has to survive the COPY based writer untouched, including
-- backslashes, dollar quotes, tabs, empty lines and a trailing newline.
select fsm.__write_file(:'dir' || '_roundtrip.txt', E'line one\n\nbackslash \\ and \\n literal\n$$ do $tag$ x $tag$ $$\ttab\nlast\n');
select pg_read_file(:'dir' || '_roundtrip.txt') = E'line one\n\nbackslash \\ and \\n literal\n$$ do $tag$ x $tag$ $$\ttab\nlast\n' as roundtrip_exact;

-- COPY terminates every row, so content that does not end in a newline gets
-- one. Documented rather than fixed: everything this extension writes ends in
-- a newline already.
select fsm.__write_file(:'dir' || '_nonewline.txt', 'abc');
select pg_read_file(:'dir' || '_nonewline.txt') = E'abc\n' as trailing_newline_added;

-- writing again replaces rather than appends
select fsm.__write_file(:'dir' || '_nonewline.txt', 'xyz');
select pg_read_file(:'dir' || '_nonewline.txt') as replaced;

-- a missing output directory is reported with the directory that is missing
select fsm.__write_file('/tmp/pg_statecharts_no_such_dir/out.sql', 'x');

-- carriage returns cannot survive the writer, so they are rejected up front
select fsm.__write_file(:'dir' || '_cr.txt', E'a\r\nb');

--
-- Finding files
--

select fsm.__write_file(:'chart_a',
  '<scxml xmlns="http://www.w3.org/2005/07/scxml" name="chart_a" version="1.0" initial="s">'
  '<state id="s" name="S"><onentry><script src="cb_one"/></onentry></state></scxml>');

select fsm.__write_file(:'chart_b',
  '<scxml xmlns="http://www.w3.org/2005/07/scxml" name="chart_b" version="2.1" initial="s">'
  '<state id="s" name="S"/></scxml>');

-- a single file is accepted directly
select fsm.__find_scxml_files(:'chart_a', false);

-- a path that is not an .scxml file is rejected
select fsm.__write_file('/tmp/pg_statecharts_test_notachart.txt', 'hello');
select fsm.__find_scxml_files('/tmp/pg_statecharts_test_notachart.txt', false);

-- a path that does not exist at all
select fsm.__find_scxml_files('/tmp/pg_statecharts_definitely_missing', false);

--
-- Importing
--

create function cb_one(event_payload fsm_event_payload) returns void language sql as $$ select $$;

select name, fsm.semver_text(version) as version from fsm.import_scxml_files(:'chart_a');

-- versions are padded on the way in
select name, fsm.semver_text(version) as version from fsm.import_scxml_files(:'chart_b');

-- importing the same chart twice is an error by default
select name from fsm.import_scxml_files(:'chart_a');

-- unless asked to skip
select count(*) as imported from fsm.import_scxml_files(:'chart_a', on_conflict_do_nothing => true);

-- a callback that does not exist is caught at import time, naming the file
delete from fsm.statechart;
select fsm.__write_file(:'dir' || '_missing_cb-1.0.scxml',
  '<scxml xmlns="http://www.w3.org/2005/07/scxml" name="missing_cb" version="1.0" initial="s">'
  '<state id="s" name="S"><onentry><script src="no_such_callback"/></onentry></state></scxml>');
select name from fsm.import_scxml_files(:'dir' || '_missing_cb-1.0.scxml');

-- nothing was left behind by the failed import
select count(*) as charts from fsm.statechart;

-- a malformed document names the file it came from
select fsm.__write_file(:'dir' || '_broken-1.0.scxml', 'this is not xml at all');
select name from fsm.import_scxml_files(:'dir' || '_broken-1.0.scxml');

-- so does a document whose root is not <scxml>
select fsm.__write_file(:'dir' || '_wrongroot-1.0.scxml', '<html><body/></html>');
select name from fsm.import_scxml_files(:'dir' || '_wrongroot-1.0.scxml');

--
-- Generating migrations
--

-- the sqitch plan file has to exist
select fsm.gen_statechart_sqitch_migrations(:'chart_a', '/tmp/pg_statecharts_no_plan.plan');

-- and it has to look like a plan
select fsm.__write_file('/tmp/pg_statecharts_test.plan', E'not a plan\n');
select fsm.gen_statechart_sqitch_migrations(:'chart_a', '/tmp/pg_statecharts_test.plan');

-- with a real plan, all three missing output directories are reported at once
-- and nothing is written
select fsm.__write_file('/tmp/pg_statecharts_test.plan', E'%syntax-version=1.0.0\n%project=testproj\n\n');
select fsm.gen_statechart_sqitch_migrations(:'chart_a', '/tmp/pg_statecharts_test.plan');

-- the plan was not touched by the failed run
select pg_read_file('/tmp/pg_statecharts_test.plan');

-- A chart with no transitions still produces a migration that runs, rather
-- than a NULL body that silently writes nothing.
select fsm.__migration_deploy(
  '<scxml xmlns="http://www.w3.org/2005/07/scxml" name="lonely" version="1.0" initial="s">'
  '<state id="s" name="S"/></scxml>'::xml, 'proj', 'lonely', '1.0');

-- and a null body is refused loudly rather than silently skipped
select fsm.__write_file('/tmp/pg_statecharts_test_null.txt', null);

drop function cb_one(fsm_event_payload);

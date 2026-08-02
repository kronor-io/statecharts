-- The fsm_semver domain that replaced the semver extension.

-- Error CONTEXT carries plpgsql line numbers, which would make this
-- expected output break on every unrelated edit.
\set SHOW_CONTEXT never
create extension if not exists pg_statecharts cascade;

-- padding of omitted components
select to_semver('1'), to_semver('1.2'), to_semver('1.2.3');

-- rejected versions
do $$
begin
  perform to_semver('1.2.3.4');
  raise exception 'should have been rejected';
exception when others then
  raise notice 'rejected 1.2.3.4';
end $$;

do $$
begin
  perform to_semver('banana');
  raise exception 'should have been rejected';
exception when others then
  raise notice 'rejected banana';
end $$;

do $$
begin
  perform '1.2'::fsm_semver;
  raise exception 'should have been rejected';
exception when check_violation then
  raise notice 'domain rejects an unpadded literal';
end $$;

select to_semver(null) is null as null_passes_through;

-- sorting has to be numeric, not lexicographic: 1.10.0 outranks 1.9.0
select version
from (values (to_semver('1.9.0')), (to_semver('1.10.0')), (to_semver('1.2.3')), (to_semver('2.0.0'))) v(version)
order by fsm.semver_sort_key(version) desc;

-- get_latest_statechart has to agree
insert into fsm.statechart (name, version) values
  ('chart', to_semver('1.9')), ('chart', to_semver('1.10.0')), ('chart', to_semver('2.0.0')), ('chart', to_semver('10.0.0'));

select name, version from fsm.get_latest_statechart('chart');

-- the sort key is immutable, so it can be indexed
create index idx_statechart_version on fsm.statechart (name, fsm.semver_sort_key(version));
drop index fsm.idx_statechart_version;

-- equality still works the way generated migrations rely on
select count(*) as found from fsm.statechart where name = 'chart' and version = to_semver('1.10.0');

-- fsm_semver is a domain over text, so it reaches clients as plain text
select version::text = '2.0.0' as text_roundtrip from fsm.statechart where version = to_semver('2');

delete from fsm.statechart;

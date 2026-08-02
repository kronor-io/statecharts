-- Turning .scxml files into sqitch migrations.
--
-- The generated files are byte for byte what the previous Rust implementation
-- produced, so regenerating an existing chart does not show up as a diff.

-- The sqitch change name for a chart, e.g. statechart/billing/checkout-1.2.0.
-- Dots in the chart name become directory separators.
create or replace function fsm.__migration_name(chart_name text, chart_version text)
returns text as
$$
  select 'statechart/' || replace(chart_name, '.', '/') || '-' || chart_version
$$ language sql immutable strict;

-- Renders a callback array as SQL row literals: ('public', 'do_thing')
create or replace function fsm.__callback_literals(callbacks fsm_callback_name[])
returns text as
$$
  select coalesce(
    string_agg(
      '(' || quote_literal(c.schema_name) || ', ' || quote_literal(c.function_name) || ')',
      ',' order by c.ord
    ),
    ''
  )
  from unnest(callbacks) with ordinality as c(schema_name, function_name, ord)
$$ language sql immutable strict;

-- The "deploy" half of the migration: insert the chart, its states and its
-- transitions.
create or replace function fsm.__migration_deploy(
  doc xml,
  project text,
  chart_name text,
  chart_version text
)
returns text as
$deploy$
  declare
    state_rows text;
    transition_rows text;
  begin
    select string_agg(
      '(chart, '
        || quote_literal(s.id) || ', '
        || quote_literal(s.name) || ', '
        || coalesce(quote_literal(s.parent_id), 'null') || ', '
        || s.is_initial::text || ', '
        || s.is_final::text || ', '
        || 'array[' || fsm.__callback_literals(s.on_entry) || ']::fsm_callback_name[], '
        || 'array[' || fsm.__callback_literals(s.on_exit) || ']::fsm_callback_name[])',
      E',\n' order by s.ord
    )
    into state_rows
    from fsm.scxml_states(doc)
      with ordinality as s(id, name, parent_id, is_initial, is_final, on_entry, on_exit, ord);

    select string_agg(
      '(chart, '
        || quote_literal(t.event) || ', '
        || quote_literal(t.source_state) || ', '
        || quote_literal(t.target_state) || ')',
      E',\n' order by t.ord
    )
    into transition_rows
    from fsm.scxml_transitions(doc)
      with ordinality as t(event, source_state, target_state, ord);

    return
      '-- Deploy ' || project || ':'
        || fsm.__migration_name(chart_name, chart_version) || ' to pg' || E'\n'
      || E'\n'
      || '-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN' || E'\n'
      || E'\n'
      || 'BEGIN;' || E'\n'
      || 'do $$' || E'\n'
      || 'declare' || E'\n'
      || 'chart bigint;' || E'\n'
      || 'begin' || E'\n'
      || 'insert into fsm.statechart (name, version) values ('
        || quote_literal(chart_name) || ', to_semver('
        || quote_literal(chart_version) || ')) returning id into chart;' || E'\n'
      || 'insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values' || E'\n'
      || state_rows || ';' || E'\n'
      -- A chart with no transitions at all is degenerate but should still
      -- produce a migration that runs, so the insert is left out entirely
      -- rather than emitted with an empty VALUES list.
      || case
           when transition_rows is null then ''
           else 'insert into fsm.transition (statechart_id, event, source_state, target_state) values' || E'\n'
                || transition_rows || ';' || E'\n'
         end
      || 'end' || E'\n'
      || '$$;' || E'\n'
      || 'COMMIT;' || E'\n';
  end;
$deploy$ language plpgsql immutable;

create or replace function fsm.__migration_revert(
  project text,
  chart_name text,
  chart_version text
)
returns text as
$$
  select
    '-- Revert ' || project || ':'
      || fsm.__migration_name(chart_name, chart_version) || ' from pg' || E'\n'
    || E'\n'
    || '-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN' || E'\n'
    || E'\n'
    || E'\n'
    || 'BEGIN;' || E'\n'
    || E'\n'
    || 'with chart as (' || E'\n'
    || '    delete from fsm.statechart' || E'\n'
    || '    where name = ' || quote_literal(chart_name) || E'\n'
    || '    and version = to_semver(' || quote_literal(chart_version) || ')' || E'\n'
    || '    returning id' || E'\n'
    || ')' || E'\n'
    || 'delete from fsm.state' || E'\n'
    || '    where statechart_id = (select id from chart);' || E'\n'
    || E'\n'
    || 'COMMIT;' || E'\n'
$$ language sql immutable strict;

create or replace function fsm.__migration_verify(
  project text,
  chart_name text,
  chart_version text
)
returns text as
$verify$
  select
    '-- Verify ' || project || ':'
      || fsm.__migration_name(chart_name, chart_version) || ' on pg' || E'\n'
    || E'\n'
    || '-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN' || E'\n'
    || E'\n'
    || 'BEGIN;' || E'\n'
    || E'\n'
    || '-- Verify that the statechart is added' || E'\n'
    || 'select 1 / count(*)' || E'\n'
    || 'from fsm.statechart' || E'\n'
    || 'where' || E'\n'
    || '    name = ' || quote_literal(chart_name) || E'\n'
    || '    and version = to_semver(' || quote_literal(chart_version) || ');' || E'\n'
    || E'\n'
    || '-- Verify that the functions that the statechart depends on exist' || E'\n'
    || 'do $$' || E'\n'
    || 'declare' || E'\n'
    || '    missing_funcs_count_ int;' || E'\n'
    || '    missing_funcs_ text;' || E'\n'
    || 'begin' || E'\n'
    || E'\n'
    || 'select' || E'\n'
    || '  string_agg(distinct format(''%s.%s'', schema_name, function_name), '', ''),' || E'\n'
    || '  count(*)' || E'\n'
    || 'into' || E'\n'
    || '  missing_funcs_,' || E'\n'
    || '  missing_funcs_count_' || E'\n'
    || 'from fsm.statechart' || E'\n'
    || 'join fsm.state' || E'\n'
    || '    on state.statechart_id = statechart.id' || E'\n'
    || ', lateral unnest(on_entry || on_exit)' || E'\n'
    || 'where' || E'\n'
    || '  statechart.name = ' || quote_literal(chart_name) || E'\n'
    || '  and statechart.version = to_semver(' || quote_literal(chart_version) || ')' || E'\n'
    || '  and not exists (' || E'\n'
    || '    select 1' || E'\n'
    || '    from pg_proc p' || E'\n'
    || '    join pg_namespace n' || E'\n'
    || '      on p.pronamespace = n.oid' || E'\n'
    || '    where' || E'\n'
    || '      n.nspname = schema_name' || E'\n'
    || '      and p.proname = function_name' || E'\n'
    || '      and p.pronargs = 1' || E'\n'
    || '      and p.proargtypes[0] = ''fsm_event_payload''::regtype::oid' || E'\n'
    || '  );' || E'\n'
    || E'\n'
    || 'if missing_funcs_count_ > 0 then' || E'\n'
    || '  raise exception' || E'\n'
    || '    $err$' || E'\n'
    || E'\n'
    || '    One or more missing or invalid functions: %' || E'\n'
    || '    All functions must take exactly one argument of the type fsm_event_payload' || E'\n'
    || E'\n'
    || '    $err$, missing_funcs_;' || E'\n'
    || 'end if;' || E'\n'
    || E'\n'
    || 'end' || E'\n'
    || '$$;' || E'\n'
    || E'\n'
    || 'ROLLBACK;' || E'\n'
$verify$ language sql immutable strict;

-- Generates a sqitch migration for every .scxml file at source_path and adds
-- it to the sqitch plan.
create or replace function fsm.gen_statechart_sqitch_migrations(
  source_path text,
  sqitch_plan_file_path text,
  recursive boolean default false
)
returns void as
$gen$
  declare
    plan_contents text;
    plan_project text;
    sqitch_dir text;
    plan_additions text := '';
    migration record;
    timestamp_str text;
  begin
    if pg_stat_file(sqitch_plan_file_path, true) is null then
      raise exception 'sqitch plan file does not exist: %', sqitch_plan_file_path
        using hint = 'the path is resolved on the database host, so when '
                     'Postgres runs in a container it has to be the path '
                     'inside the container';
    end if;

    begin
      plan_contents := pg_read_file(sqitch_plan_file_path);
    exception when others then
      raise exception 'failed to read sqitch plan file %: %',
        sqitch_plan_file_path, sqlerrm;
    end;

    select trim(substring(line from '^%project=(.*)$'))
    into plan_project
    from unnest(string_to_array(plan_contents, E'\n')) as line
    where line ~ '^%project='
    limit 1;

    if plan_project is null or plan_project = '' then
      raise exception 'could not find a %%project= line in %', sqitch_plan_file_path
        using hint = 'is this really a sqitch plan file?';
    end if;

    -- The migrations go next to the plan file. A bare file name means the data
    -- directory, which is where the server resolves relative paths.
    if sqitch_plan_file_path !~ '/' then
      sqitch_dir := '.';
    else
      sqitch_dir := coalesce(
        nullif(regexp_replace(sqitch_plan_file_path, '/[^/]*$', ''), ''),
        '/'
      );
    end if;

    -- Everything is parsed and rendered up front so that a chart that fails to
    -- parse does not leave half the migrations written to disk.
    if to_regclass('pg_temp.__pg_statecharts_migrations') is null then
      create temp table __pg_statecharts_migrations (
        file_path text,
        migration_name text,
        deploy text,
        revert text,
        verify text
      ) on commit drop;
    else
      truncate __pg_statecharts_migrations;
    end if;

    declare
      file_path text;
      doc xml;
      chart_name text;
      chart_version text;
    begin
      for file_path in
        select * from fsm.__find_scxml_files(source_path, recursive) order by 1
      loop
        doc := fsm.__read_scxml(file_path);
        chart_name := fsm.__scxml_name(doc, file_path);
        chart_version := fsm.__scxml_version(doc, file_path);

        insert into __pg_statecharts_migrations values (
          file_path,
          fsm.__migration_name(chart_name, chart_version),
          fsm.__migration_deploy(doc, plan_project, chart_name, chart_version),
          fsm.__migration_revert(plan_project, chart_name, chart_version),
          fsm.__migration_verify(plan_project, chart_name, chart_version)
        );
      end loop;
    end;

    if not exists (select 1 from __pg_statecharts_migrations) then
      raise exception 'no .scxml files found at %', source_path
        using hint = case
          when recursive then 'check the path'
          else 'pass recursive => true to also look in subdirectories'
        end;
    end if;

    -- Check every output directory before writing anything, so that a missing
    -- verify/ directory cannot leave a half written set of migrations behind.
    declare
      missing text;
    begin
      select string_agg(distinct dir, E'\n' order by dir)
      into missing
      from __pg_statecharts_migrations m
      cross join lateral (
        values
          (sqitch_dir || '/deploy/' || m.migration_name),
          (sqitch_dir || '/revert/' || m.migration_name),
          (sqitch_dir || '/verify/' || m.migration_name)
      ) as paths(path)
      cross join lateral (
        select regexp_replace(paths.path, '/[^/]*$', '') as dir
      ) as d
      where pg_stat_file(d.dir, true) is null;

      if missing is not null then
        raise exception 'the sqitch project is missing output directories'
          using detail = missing,
                hint = format(
                  'create them first, for example: mkdir -p %s/{deploy,revert,verify}/statechart',
                  sqitch_dir
                );
      end if;
    end;

    timestamp_str := to_char(now() at time zone 'utc', 'YYYY-MM-DD"T"HH24:MI:SS"Z"');

    for migration in
      select * from __pg_statecharts_migrations order by migration_name
    loop
      perform fsm.__write_file(sqitch_dir || '/deploy/' || migration.migration_name || '.sql', migration.deploy);
      perform fsm.__write_file(sqitch_dir || '/revert/' || migration.migration_name || '.sql', migration.revert);
      perform fsm.__write_file(sqitch_dir || '/verify/' || migration.migration_name || '.sql', migration.verify);

      -- A sqitch change line starts with the change name followed by
      -- whitespace, so the first token is enough to tell whether the plan
      -- already knows about this migration.
      if exists (
        select 1
        from unnest(string_to_array(plan_contents, E'\n')) as line
        where split_part(line, ' ', 1) = migration.migration_name
      ) then
        raise info 'updated existing migration: %', migration.migration_name;
      else
        plan_additions := plan_additions
          || migration.migration_name || ' ' || timestamp_str
          || ' pg_statecharts <pg_statecharts@postgres> # '
          || migration.migration_name || E'\n';

        raise info 'created new migration: %', migration.migration_name;
      end if;
    end loop;

    if plan_additions <> '' then
      -- Guard against a plan file that does not end in a newline, which would
      -- otherwise glue the first new change onto the last existing one.
      if plan_contents <> '' and right(plan_contents, 1) <> E'\n' then
        plan_contents := plan_contents || E'\n';
      end if;

      perform fsm.__write_file(sqitch_plan_file_path, plan_contents || plan_additions);
    end if;
  end;
$gen$ language plpgsql volatile;

comment on function fsm.gen_statechart_sqitch_migrations(text, text, boolean) is $comment$
    Generates a sqitch deploy/revert/verify migration for every .scxml file at
    source_path and registers it in the sqitch plan.

    Both paths are resolved on the database host rather than on the client, and
    the deploy/statechart, revert/statechart and verify/statechart directories
    have to already exist.

    Requires the pg_read_server_files and pg_write_server_files roles (or
    superuser). Note that the files are written by the postgres process, so run
    the database as your own user if you want to own the generated files:

        docker compose run --user "$(id -u):$(id -g)" ...
$comment$;

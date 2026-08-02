-- Loads .scxml files straight into fsm.statechart / fsm.state / fsm.transition.
--
-- This is the shortcut for iterating on a chart locally. Use
-- fsm.gen_statechart_sqitch_migrations when the chart should be committed and
-- deployed like any other schema change.
--
-- Everything happens in the caller's transaction, so a failure part way
-- through leaves nothing behind.
create or replace function fsm.import_scxml_files(
  source_path text,
  recursive boolean default false,
  on_conflict_do_nothing boolean default false
)
returns setof fsm.statechart as
$$
  declare
    file_path text;
    doc xml;
    chart fsm.statechart%rowtype;
    chart_name text;
    chart_version text;
    missing_functions text;
    err_message text;
    err_detail text;
    err_hint text;
    err_code text;
  begin
    for file_path in
      select * from fsm.__find_scxml_files(source_path, recursive) order by 1
    loop
      begin
        doc := fsm.__read_scxml(file_path);
        chart_name := fsm.__scxml_name(doc, file_path);
        chart_version := fsm.__scxml_version(doc, file_path);

        if on_conflict_do_nothing then
          insert into fsm.statechart (name, version)
          values (chart_name, to_semver(chart_version))
          on conflict do nothing
          returning statechart.* into chart;

          -- already imported, leave the existing chart untouched
          if not found then
            continue;
          end if;
        else
          begin
            insert into fsm.statechart (name, version)
            values (chart_name, to_semver(chart_version))
            returning statechart.* into chart;
          exception when unique_violation then
            raise exception '% version % has already been imported',
              chart_name, chart_version
              using hint = 'pass on_conflict_do_nothing => true to skip charts '
                           'that are already in the database, or bump the '
                           'version attribute in the .scxml file';
          end;
        end if;

        insert into fsm.state (
          statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit
        )
        select
          chart.id, s.id, s.name, s.parent_id, s.is_initial, s.is_final, s.on_entry, s.on_exit
        from fsm.scxml_states(doc) as s;

        insert into fsm.transition (statechart_id, event, source_state, target_state)
        select chart.id, t.event, t.source_state, t.target_state
        from fsm.scxml_transitions(doc) as t;

        -- The on_entry/on_exit callbacks are looked up by name at run time, so
        -- a typo would otherwise only show up when the machine reaches that
        -- state. Catch it at import time instead.
        select string_agg(distinct format('%s.%s', schema_name, function_name), ', ')
        into missing_functions
        from fsm.state
        cross join lateral unnest(state.on_entry || state.on_exit)
        where state.statechart_id = chart.id
          and not exists (
            select 1
            from pg_proc p
            join pg_namespace n on p.pronamespace = n.oid
            where n.nspname = schema_name
              and p.proname = function_name
              and p.pronargs = 1
              and p.proargtypes[0] = 'fsm_event_payload'::regtype::oid
          );

        if missing_functions is not null then
          raise exception '% version % references functions that do not exist: %',
            chart.name, chart.version, missing_functions
            using hint = 'every on_entry and on_exit callback has to already '
                         'exist and take exactly one fsm_event_payload argument';
        end if;

        return next chart;
      exception when others then
        get stacked diagnostics
          err_code = returned_sqlstate,
          err_message = message_text,
          err_detail = pg_exception_detail,
          err_hint = pg_exception_hint;

        raise exception using
          errcode = err_code,
          message = err_message,
          detail = trim(both E'\n' from
            coalesce(err_detail, '') || E'\n' || format('while importing %s', file_path)),
          hint = err_hint;
      end;
    end loop;
  end;
$$ language plpgsql volatile;

comment on function fsm.import_scxml_files(text, boolean, boolean) is $comment$
    Reads every .scxml file at source_path and inserts it into the statechart
    tables, returning the fsm.statechart rows that were created.

    source_path may be a single file or a directory, and is resolved on the
    database host rather than on the client. Set recursive to also descend into
    subdirectories, and on_conflict_do_nothing to silently skip charts whose
    name and version are already in the database.

    Requires the pg_read_server_files role (or superuser).
$comment$;

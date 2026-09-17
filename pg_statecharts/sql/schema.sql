-- ltree has to be in public, because the triggers that fire during pg_restore
-- write it as public.ltree. pg_restore runs with an empty search_path, so a
-- bare "ltree" cannot be resolved there and the restore of the fsm data fails;
-- an explicit schema is the only way out, and it has to be a literal one.
--
-- ltree is relocatable, and CREATE EXTENSION ... CASCADE propagates its own
-- SCHEMA option to the extensions it creates, so "create extension
-- pg_statecharts schema app cascade" puts ltree in app. Without this check
-- that succeeds -- a PL/pgSQL body is not resolved at creation time -- and
-- dies on the first insert into fsm.state instead.
do $ltree_check$
declare
  ltree_schema name;
begin
  if to_regtype('public.ltree') is null then
    select n.nspname into ltree_schema
    from pg_extension e
    join pg_namespace n on n.oid = e.extnamespace
    where e.extname = 'ltree';

    raise exception 'pg_statecharts requires the ltree extension in the public schema'
      using detail = case
              when ltree_schema is null then 'ltree is not installed'
              else format('ltree is installed in schema %I', ltree_schema)
            end,
            hint = 'install pg_statecharts without a SCHEMA option, or move ltree: '
                   'alter extension ltree set schema public;';
  end if;
end
$ltree_check$;

create schema fsm;

comment on schema fsm is 'Schema containing the statecharts implementation.';

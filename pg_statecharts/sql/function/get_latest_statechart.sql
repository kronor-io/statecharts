-- `named` is the name this took before 0.2.0, kept so that callers that name
-- their arguments keep working, and removed in 0.3.0.
create or replace function fsm.get_latest_statechart(
    statechart_name text default null
  -- deprecated, removed in 0.3.0
  , named           text default null
)
returns setof fsm.statechart as
$$
    select *
    from fsm.statechart
    -- a null name matches nothing, which is the zero rows `strict` used to
    -- return before the deprecated name forced a default on
    where name = coalesce(statechart_name, named)
    -- fsm.semver is an integer array, so this is already a numeric comparison
    -- component by component: 1.10.0 outranks 1.9.0 because 10 > 9.
    order by version desc
    limit 1
$$ language sql
    stable
    parallel safe
    rows 1;

comment on function fsm.get_latest_statechart(text, text) is $comment$
    Get the latest statechart of given name.
$comment$;

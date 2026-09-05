create or replace function fsm.get_latest_statechart(named text)
returns setof fsm.statechart as
$$
    select *
    from fsm.statechart
    where name = named
    -- fsm.semver is an integer array, so this is already a numeric comparison
    -- component by component: 1.10.0 outranks 1.9.0 because 10 > 9.
    order by version desc
    limit 1
$$ language sql
    strict
    stable
    parallel safe
    rows 1;

comment on function fsm.get_latest_statechart(text) is $comment$
    Get the latest statechart of given name.
$comment$;

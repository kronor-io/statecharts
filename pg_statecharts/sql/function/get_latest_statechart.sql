create or replace function fsm.get_latest_statechart(named text)
returns setof fsm.statechart as
$$
    select *
    from fsm.statechart
    where name = named
    -- fsm.semver is a domain over text, so it has to be ordered by its numeric
    -- components rather than lexicographically. See fsm.semver_sort_key.
    order by fsm.semver_sort_key(version) desc
    limit 1
$$ language sql
    strict
    stable
    parallel safe
    rows 1;

comment on function fsm.get_latest_statechart(text) is $comment$
    Get the latest statechart of given name.
$comment$;

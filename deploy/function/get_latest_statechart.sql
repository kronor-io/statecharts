-- Deploy statecharts:function/get_latest_statechart to pg

BEGIN;

    create or replace function fsm.get_latest_statechart(named text)
    returns setof fsm.statechart as
    $$
        select *
        from fsm.statechart
        where name = named
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
COMMIT;

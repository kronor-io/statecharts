-- Deploy statecharts:function/create_machine to pg

BEGIN;

  create or replace function fsm.create_machine(shard bigint, statechart bigint)
    returns fsm.state_machine as
    $$
        insert into fsm.state_machine (shard_id, statechart_id) values
          (shard, statechart)
        returning *;
    $$ language sql volatile strict;

    comment on function fsm.create_machine(bigint, bigint) is $comment$
        creates a state machine without starting it
    $comment$;

COMMIT;

-- This is an AFTER INSERT ... FOR EACH STATEMENT trigger, and it fires at the
-- end of every statement that inserts an event, including an insert made from
-- inside a transition callback. So the rows in `new` are all of the machine's
-- unhandled events: an event is never left for a later statement to pick up.
-- That is what lets the rows be handed straight to fsm.handle_machine_events
-- instead of being looked up there; see that function for why it matters.
create or replace function fsm.trig_notify_machine_event() returns trigger as
$$
  declare
    machine_ record;
  begin

    for machine_ in
        -- Aliased: a bare `new` would clash with PL/pgSQL's own NEW record.
        select
            inserted.shard_id
          , inserted.state_machine_id
          , array_agg(
                ( inserted.shard_id
                , inserted.id
                , inserted.state_machine_id
                , inserted.created_at
                , inserted.name
                , inserted.data
                )::fsm.machine_event
                order by inserted.created_at, inserted.id
            ) as events
        from new as inserted
        group by inserted.shard_id, inserted.state_machine_id
        -- Deterministic on purpose. Each iteration writes its machine's rows
        -- in fsm.state_machine_state: exited_at stamped on the states it
        -- leaves, a row inserted for each state it enters. Nothing here takes
        -- a lock of its own, but both of those block a second transaction
        -- doing the same to that machine until this one commits -- an updated
        -- row holds its lock, and idx_state makes a duplicate active state
        -- wait on whoever inserted first. So two statements inserting events
        -- for the same machines in different orders would be able to block
        -- each other in opposite orders and deadlock. Ordering here settles
        -- it for a single statement; callbacks that send events on to further
        -- machines still pick their own order, and a deadlock between those
        -- is detected and reported as 40P01. Within one machine the events
        -- are already ordered by the array_agg above.
        order by inserted.shard_id, inserted.state_machine_id
    loop
      perform fsm.handle_machine_events(
          machine_id => machine_.state_machine_id
        , events     => machine_.events
        , shard_id   => machine_.shard_id
      );
    end loop;

    return null;
  end;
$$ language plpgsql;

comment on function fsm.trig_notify_machine_event is $comment$
  Handles all of the newly inserted events for all affected state machines.
$comment$;

set client_min_messages TO warning;
drop trigger if exists notify_machine_event on fsm.state_machine_event;
reset client_min_messages;

create trigger notify_machine_event
after insert
on fsm.state_machine_event
referencing new table as new
for each statement
execute function fsm.trig_notify_machine_event();

comment on trigger notify_machine_event on fsm.state_machine_event is $comment$
  Handles all of the newly inserted events for all affected state machines.
$comment$;

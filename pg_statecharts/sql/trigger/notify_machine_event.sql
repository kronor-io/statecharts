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

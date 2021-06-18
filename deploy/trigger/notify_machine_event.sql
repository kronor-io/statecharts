-- Deploy statecharts:trigger/notify_machine_event to pg

BEGIN;

  create or replace function fsm.trig_notify_machine_event() returns trigger as
  $$
    declare
        event record;
    begin

      for event in select distinct shard_id, state_machine_id from new
      loop
        perform pg_notify('fsm_machine_events', row_to_json(event)::text);
      end loop;

      return null;
    end;
  $$ language plpgsql;

  set client_min_messages TO warning;
  drop trigger if exists notify_machine_event on fsm.state_machine_event;
  reset client_min_messages;

  create trigger notify_machine_event
  after insert
  on fsm.state_machine_event
  referencing new table as new
  for each statement
  execute function fsm.trig_notify_machine_event();

COMMIT;

-- Deploy example:combination_lock to pg

BEGIN;

  create table combination_lock (
    id bigint not null generated always as identity,
    state_machine_id bigint not null,
    materialized_status text not null default 'zero_correct',
    unlocked_until timestamptz
  );

  create index if not exists idx_combination_lock_state_machine_id
    on combination_lock (state_machine_id);

  -- Trigger to set up state machine (before insert to please the 'not null'
  -- constraint)
  create or replace function trig_create_combination_lock_flow()
  returns trigger as
  $$
      begin
          if new.state_machine_id is null
          then
              select s.id into new.state_machine_id
              from fsm.create_state_machine_with_latest_statechart(
                  shard_id_ => 1,
                  named => 'combination_lock_flow'
              ) s;
          end if;
          return new;
      end
  $$ language plpgsql volatile security definer;

  create or replace trigger create_combination_lock_flow
  before insert
  on combination_lock
  for each row
  execute function trig_create_combination_lock_flow();

  -- Trigger to _start_ the state machine (after insert to not run into
  -- problems with the row not existing when the state machine starts
  -- executing)
  create or replace function trig_start_combination_lock_flow()
  returns trigger as
  $$
      begin

          perform fsm.start_machine(
              shard => 1,
              machine_id => new.state_machine_id
          )
          from combination_lock
          where id = new.id;

          return null;
      end
  $$ language plpgsql volatile security definer;

  create or replace trigger start_combination_lock_flow
  after insert
  on combination_lock
  for each row
  execute function trig_start_combination_lock_flow();

  -- Functions used by the state machine
  create or replace function materialize_combination_lock_status(event_payload fsm_event_payload)
      returns void as
  $$
      begin
          update combination_lock
          set materialized_status = (event_payload).to_state
          where
            state_machine_id = (event_payload).machine_id;
      end
  $$ language plpgsql;

  create or replace function unlock_combination_lock(event_payload fsm_event_payload)
      returns void as
  $$
      begin
          update combination_lock
          set unlocked_until = now() + interval '10 seconds'
          where
            state_machine_id = (event_payload).machine_id;

          perform fsm.notify_state_machine(
            shard => (event_payload).shard_id,
            machine => (event_payload).machine_id,
            event => 'combination_lock.reset'
          );
      end
  $$ language plpgsql;
COMMIT;

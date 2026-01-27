-- Deploy example:lightswitch to pg

BEGIN;

  -- Initialize the extension with cascade to make sure that dependencies are
  -- installed
  create extension if not exists pg_statecharts cascade;

  -- Create lightswitch table to represent a switch that has two states, ON
  -- and OFF. When it's ON it turned_on_at will be not null and when it's OFF
  -- turned_off_at will be not null
  create table if not exists lightswitch (
    id bigint not null generated always as identity,
    state_machine_id bigint not null,
    turned_on_at timestamptz,
    turned_off_at timestamptz,

    primary key (id)
  );

  -- It's a good idea to index on the state machine id as the state machine
  -- will use that column to find its lightswitch row
  create index if not exists idx_lightswitch_state_machine_id
    on lightswitch (state_machine_id);

  -- Trigger to set up state machine (before insert to please the 'not null'
  -- constraint)
  create or replace function trig_create_lightswitch_flow()
  returns trigger as
  $$
      begin
          if new.state_machine_id is null
          then
              select s.id into new.state_machine_id
              from fsm.create_state_machine_with_latest_statechart(
                  1,
                  'lightswitch_flow'
              ) s;
          end if;
          return new;
      end
  $$ language plpgsql volatile security definer;

  create trigger create_lightswitch_flow
  before insert
  on lightswitch
  for each row
  execute function trig_create_lightswitch_flow();

  -- Trigger to _start_ the state machine (after insert to not run into
  -- problems with the row not existing when the state machine starts
  -- executing)
  create or replace function trig_start_lightswitch_flow()
  returns trigger as
  $$
      begin

          perform fsm.start_machine(
              1,
              new.state_machine_id
          )
          from lightswitch
          where id = new.id;

          return null;
      end
  $$ language plpgsql volatile security definer;

  create trigger start_lightswitch_flow
  after insert
  on lightswitch
  for each row
  execute function trig_start_lightswitch_flow();

  -- Functions used by the state machine
  create or replace function turn_on_lightswitch(event_payload fsm_event_payload)
    returns void as
    $$
      update lightswitch set
        turned_on_at = now(),
        turned_off_at = null
      where
        state_machine_id = (event_payload).machine_id;
    $$
    language sql;

  create or replace function turn_off_lightswitch(event_payload fsm_event_payload)
    returns void as
    $$
      update lightswitch set
        turned_on_at = null,
        turned_off_at = now()
      where
        state_machine_id = (event_payload).machine_id;
    $$
    language sql;

COMMIT;

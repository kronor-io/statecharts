-- Deploy statecharts:trigger/check_valid_initial_state to pg
BEGIN;

  create or replace function fsm.trig_check_valid_initial_state() returns trigger as
  $$
    declare
        total_initial int;
    begin
      select count(*) into total_initial
      from fsm.get_initial_state(coalesce(NEW.statechart_id, OLD.statechart_id));

      if total_initial <> 1
      then
        raise exception 'exactly one initial state is required per statechart';
      end if;

      return null;
    end;
  $$ language plpgsql;

  set client_min_messages TO warning;
  drop trigger if exists check_valid_initial_state_insert on fsm.state;
  drop trigger if exists check_valid_initial_state_update on fsm.state;
  drop trigger if exists check_valid_initial_state_delete on fsm.state;
  reset client_min_messages;

  -- By making the triggers INITIALLY DEFERRED, we tell PostgreSQL
  -- to check the condition at COMMIT time. This means that inside a
  -- single transaction we are allowed to be inconsistent, as long as
  -- the final result remains consistent.

  create constraint trigger check_valid_initial_state_insert
  after insert
  on fsm.state
  deferrable initially deferred
  for each row
  when (NEW.is_initial and NEW.parent_id is null)
  execute function fsm.trig_check_valid_initial_state();

  create constraint trigger check_valid_initial_state_update
  after update
  on fsm.state
  deferrable initially deferred
  for each row
  when (
    NEW.is_initial <> OLD.is_initial or
    NEW.statechart_id <> OLD.statechart_id or
    NEW.parent_id is distinct from OLD.parent_id
  )
  execute function fsm.trig_check_valid_initial_state();

  create constraint trigger check_valid_initial_state_delete
  after delete
  on fsm.state
  deferrable initially deferred
  for each row
  when (OLD.is_initial and OLD.parent_id is null)
  execute function fsm.trig_check_valid_initial_state();

COMMIT;

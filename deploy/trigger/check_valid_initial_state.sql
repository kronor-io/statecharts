-- Deploy statecharts:trigger/check_valid_initial_state to pg
BEGIN;

  create or replace function fsm.trig_check_valid_initial_state() returns trigger as
  $$
    declare
        chart bigint;
        total_initial bigint;
    begin
      for chart in
        select distinct statechart_id
        from changed
        where exists (select 1 from fsm.statechart where id = changed.statechart_id)
      loop
          select count(*) into total_initial
          from fsm.state
          where statechart_id = chart
            and is_initial
            and parent_id is null;

          if total_initial is distinct from 1
          then
            raise exception 'exactly one initial state is required per statechart';
          end if;

      end loop;

      return null;
    end;
  $$ language plpgsql;

  comment on function fsm.trig_check_valid_initial_state is $comment$
      Checks that there is only initial state per statechart. Only considers
      initial states whose parent_id is null.
  $comment$;

  set client_min_messages TO warning;
  drop trigger if exists check_valid_initial_state_insert on fsm.state;
  drop trigger if exists check_valid_initial_state_update on fsm.state;
  drop trigger if exists check_valid_initial_state_delete on fsm.state;
  reset client_min_messages;

  create trigger check_valid_initial_state_insert
  after insert
  on fsm.state
  referencing new table as changed
  for each statement
  execute function fsm.trig_check_valid_initial_state();

  comment on trigger check_valid_initial_state_insert on fsm.state is $comment$
      Checks that there is only initial state per statechart. Only considers
      initial states whose parent_id is null.
  $comment$;

  create trigger check_valid_initial_state_update
  after update
  on fsm.state
  referencing new table as changed
  for each statement
  execute function fsm.trig_check_valid_initial_state();

  comment on trigger check_valid_initial_state_update on fsm.state is $comment$
      Checks that there is only initial state per statechart. Only considers
      initial states whose parent_id is null.
  $comment$;

  create trigger check_valid_initial_state_delete
  after delete
  on fsm.state
  referencing old table as changed
  for each statement
  execute function fsm.trig_check_valid_initial_state();

  comment on trigger check_valid_initial_state_delete on fsm.state is $comment$
      Checks that there is only initial state per statechart. Only considers
      initial states whose parent_id is null.
  $comment$;

COMMIT;

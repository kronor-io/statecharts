-- Verify statecharts:function/handle_machine_events on pg

BEGIN;

  select  has_function_privilege('fsm.handle_machine_events(bigint, bigint)', 'execute');

  do $$
    declare
      err text;
    begin

      select string_agg(
        errors, '
        ') into err -- the string with a new line is on purpose

      from

      plpgsql_check_function('fsm.handle_machine_events(bigint, bigint)') errors;

      if err <> ''
      then
        raise exception '%', err;
      end if;

    end;
    $$;


ROLLBACK;

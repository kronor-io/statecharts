-- Verify statecharts:function/start_machine on pg

BEGIN;

  select  has_function_privilege('fsm.start_machine(bigint, bigint, jsonb)', 'execute');

  do $$
    declare
      err text;
    begin

      select string_agg(
        errors, '
        ') into err -- the string with a new line is on purpose

      from

      plpgsql_check_function('fsm.start_machine(bigint, bigint, jsonb)') errors;

      if err <> ''
      then
        raise exception '%', err;
      end if;

    end;
    $$;

ROLLBACK;

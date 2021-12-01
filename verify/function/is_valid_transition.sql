-- Verify statecharts:function/is_valid_transition on pg

BEGIN;

  select  has_function_privilege('fsm.is_valid_transition(bigint, bigint, text)', 'execute');

  do $$
    declare
      err text;
    begin

      select string_agg(
        errors, '
        ') into err -- the string with a new line is on purpose

      from

      plpgsql_check_function('fsm.is_valid_transition(bigint, bigint, text)') errors;

      if err <> ''
      then
        raise exception '%', err;
      end if;

    end;
    $$;

ROLLBACK;

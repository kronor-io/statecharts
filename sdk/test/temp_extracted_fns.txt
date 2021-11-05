
----------------------------------------------------
-- So we can know that an action that was specified in the charts actually exists in the database.
create or replace function function_exists(schema text, fname text) returns bool as
$$ BEGIN
      IF NOT EXISTS (SELECT * FROM information_schema.routines WHERE routine_name = fname) -- TODO how constraint per schema?
                                                                                          -- TODO you can also constraint by type if you feel like it
      THEN           RETURN FALSE;
      ELSE           RETURN TRUE;
      END IF;
    END
$$ language plpgsql strict;

----------------------------------------------------
-- This table exists so we can check that a action was
-- called before we actually call the function, and we
-- register this call so we can scrutinize it down the
-- road.
create temporary table intercepted (id bigint not null generated always as identity,event text not null,event_date timestamptz not null default now());
-- We use this to check what was the last called action.
create or replace function last_intercepted() returns text as
$$
  declare
    foo intercepted;
  begin
    select * into foo from intercepted order by id desc limit 1;
    return foo.event;
    end
$$ language plpgsql strict;
-- We use this to register a new action in the table. Just a helper. This is functional sql now, I call the shots here.
create or replace function intercepted_(event_ text) returns void as
$$ begin insert into intercepted (event) values (event_); end $$ language plpgsql strict;

-----------------------------------------------------------------------------------------------
--
--                                        :::::::::::
--                                        : WARNING :
--                                        :::::::::::
--
--                                   DO NOT EDIT THIS FILE
--                               IT WAS AUTOMATICALLY GENERATED
--                                CHANGES MAY BE OVERWRITTEN
--
-----------------------------------------------------------------------------------------------
BEGIN;
      select plan(4);

      -- This table exists so we can check that a action was 
      -- called before we actually call the function, and we 
      -- register this call so we can scrutinize it down the 
      -- road.
      create temporary table intercepted (id bigint not null generated always as identity,event text not null,event_date timestamptz not null default now());

      -- We use this to check what was the last called action.
      create or replace function last_intercepted() returns intercepted as 
      $$ begin return * from intercepted order by id desc limit 1; end $$ language plpgsql strict;

      -- We use this to register a new action in the table. Just a helper. This is functional sql now, I call the shots here.
      create or replace function intercepted_(event_ text) returns void as 
      $$ begin insert into intercepted (event) values (event_); end $$ language plpgsql strict;

      -- So we can know that an action that was specified in the charts actually exists in the database.
      create or replace function function_exists(schema text not null, fname text not null) returns bool as
      $$ BEGIN 
           IF NOT EXISTS (SELECT * FROM information_schema.routines WHERE routine_name = fname) -- TODO how constraint per schema?
                                                                                                -- TODO you can also constraint by type if you feel like it
           THEN           RETURN FALSE; 
           ELSE           RETURN TRUE; 
           END IF;       
         END 
      $$ language plpgsql strict;

      select is(function_exists('invoice','created_action'), true);
      select is(function_exists('invoice','soft_reminder_action'), true);
      select is(function_exists('invoice','due_date_action'), true);
      select is(function_exists('invoice','reminder1_action'), true);
      select is(function_exists('invoice','reminder2_action'), true);
      select is(function_exists('invoice','paid_more_than_min_action'), true);
      select is(function_exists('invoice','transferring_to_account_action'), true);
      select is(function_exists('invoice','invoice_settled_action'), true);

      create function invoice.created_action(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('invoice.created_action'); return; end; $$ language plpgsql volatile strict;
      create function invoice.due_date_action(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('invoice.due_date_action'); return; end; $$ language plpgsql volatile strict;
      create function invoice.reminder1_action(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('invoice.reminder1_action'); return; end; $$ language plpgsql volatile strict;
      create function invoice.reminder2_action(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('invoice.reminder2_action'); return; end; $$ language plpgsql volatile strict;
      create function invoice.paid_more_than_min_action(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('invoice.paid_more_than_min_action'); return; end; $$ language plpgsql volatile strict;
      create function invoice.transferring_to_account_action(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('invoice.transferring_to_account_action'); return; end; $$ language plpgsql volatile strict;
      create function invoice.settled_action(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('invoice.settled_action'); return; end; $$ language plpgsql volatile strict;
      create function invoice.soft_reminder_action(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('invoice.soft_reminder_action'); return; end; $$ language plpgsql volatile strict;

      -- TODO check if the machine exists, as well, in the version that we expects
      -- TODO we just need a test for each transition
      -- TODO together with that or replacing that we can also use the function is_valid_transition to check all valid transitions for a state

      -- TODO but how do we create the new machine? we create one every time? why not?

      -- TODO one good thing or following the normal path would be testing the if states are substates of the correct parents and stuff like that.

      -- NOTE new cluster
      -- insert into
      -- notify_state_machine
      -- select is() ... from

      alter function invoice.created_action(event_payload fsm_event_payload) rename to ORIGINAL_created_action;
      alter function invoice.soft_reminder_action(event_payload fsm_event_payload) rename to ORIGINAL_soft_reminder_action;
      alter function invoice.due_date_action(event_payload fsm_event_payload) rename to ORIGINAL_due_date_action;
      alter function invoice.reminder1_action(event_payload fsm_event_payload) rename to ORIGINAL_reminder1_action;
      alter function invoice.reminder2_action(event_payload fsm_event_payload) rename to ORIGINAL_reminder2_action;
      alter function invoice.paid_more_than_min_action(event_payload fsm_event_payload) rename to ORIGINAL_paid_more_than_min_action;
      alter function invoice.transferring_to_account_action(event_payload fsm_event_payload) rename to ORIGINAL_transferring_to_account_action;
      alter function invoice.invoice_settled_action(event_payload fsm_event_payload) rename to ORIGINAL_settled_action;
      select id as machine_id from fsm.start_machine_with_latest_statechart(1, 'invoice_flow') \gset
      select count(*) from fsm.state_machine_state where state_id = 'settling';
      select count(*) from fsm.state_machine_state where state_id = 'in_progress';
      select count(*) from fsm.state_machine_state where state_id = 'created';
      with last_ as (select * from last_intercepted()) select is(last_.event, 'invoice.created_action'::text) from last_;
      select fsm.notify_state_machine(1::bigint, :machine_id::bigint, 'invoice.time.soft_reminder');
      select count(*) from fsm.state_machine_state where state_id = 'settling';
      select count(*) from fsm.state_machine_state where state_id = 'in_progress';
      select count(*) from fsm.state_machine_state where state_id = 'in_progress_soft_reminder';
      with last_ as (select * from last_intercepted()) select is(last_.event, 'invoice.soft_reminder_action'::text) from last_;
      select finish();
ROLLBACK;
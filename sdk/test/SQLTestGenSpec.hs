module SQLTestGenSpec where

import Statechart.Analysis
import Statechart.Types
import RIO
import Test.Hspec

-- TODO we can get rid of this file

spec :: Spec
spec = return ()

-- | Example of how one path of the invoice_flow might look.
-- We can use this to create SQL tests.
-- We can use the number of tests to specify the pgTap "prove(n)"
examplePath :: Path
examplePath =

-- TODO this is probably not going to be used anymore

  [ Hop Nothing
        (StateName "settling/in_progress/created")
        ["invoice.created_action"]
  , Hop (Just (EventName "invoice.time.soft_reminder"))
        (StateName "settling/in_progress/in_progress_soft_reminder")
        ["invoice.soft_reminder_action"]
  , Hop (Just (EventName "invoice.time.due"))
        (StateName "settling/in_progress/due_date")
        ["invoice.due_date_action"]
  , Hop (Just (EventName "invoice.time.reminder1"))
        (StateName "settling/in_progress/reminder1")
        ["invoice.reminder1_action"]
  , Hop (Just (EventName "invoice.time.reminder2"))
        (StateName "settling/in_progress/reminder2")
        ["invoice.reminder2_action"]
  , Hop (Just (EventName "invoice.time.debt_collection"))
        (StateName "settling/in_progress/debt_collection_date")
        []
  , Hop (Just (EventName "invoice.pay.enough"))
        (StateName "settling/minimum_payment/paid_more_than_min")
        ["invoice.paid_more_than_min_action"]
  , Hop (Just (EventName "invoice.time.soft_reminder"))
        (StateName "settling/minimum_payment/minimum_payment_soft_reminder")
        ["invoice.soft_reminder_action"]
  , Hop (Just (EventName "invoice.time.due"))
        (StateName "settling/minimum_payment/transferring_to_account")
        ["invoice.transferring_to_account_action"]
  , Hop (Just (EventName "invoice.settle"))
        (StateName "settled")
        ["invoice.invoice_settled_action"]
  ]
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

module SQLTestGenSpec where

import Statechart.Analysis
import Statechart.Types
import RIO
import Test.Hspec

spec :: Spec
spec = return ()

-- | Example of how one path of the invoice_flow might look.
-- We can use this to create SQL tests.
-- We can use the number of tests to specify the pgTap "prove(n)"
examplePath :: Path
examplePath =
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


-- | Example of how one path of the invoice_flow might look.]
-- We can use this to create SQL tests.
-- We can use the number of tests to specify the pgTap "prove(n)"
examplePath :: Path -- TODO also maybe do not call it path since it might be confusing
-- call it transition record, maybe?
examplePath =
  -- TODO instead of doing it like this, use a single constructor for this cluster
  -- of 3 things
  [ NoNewTransition
  , State "settling/in_progress/created"
  , Action "invoice.created_action"

  , Trans "invoice.time.soft_reminder"
  , State "settling/in_progress/in_progress_soft_reminder"
  , Action "invoice.soft_reminder_action"

  , Trans "invoice.time.due"
  , State "settling/in_progress/due_date"
  , Action "invoice.due_date_action"

  , Trans "invoice.time.reminder1"
  , State "settling/in_progress/reminder1" 
  , Action "invoice.reminder1_action"

  , Trans "invoice.time.reminder2"
  , State "settling/in_progress/reminder2" 
  , Action "invoice.reminder2_action"

  , Trans "invoice.time.debt_collection"
  , State "settling/in_progress/debt_collection_date"
  , NoNewAction

  , Trans "invoice.pay.enough"
  , State "settling/minimum_payment/paid_more_than_min"
  , Action "invoice.paid_more_than_min_action"

  , Trans "invoice.time.soft_reminder"
  , State "settling/minimum_payment/minimum_payment_soft_reminder"
  , Action "invoice.soft_reminder_action"

  , Trans "invoice.time.due"
  , State "settling/minimum_payment/transferring_to_account"
  , Action "invoice.transferring_to_account_action"

  , Trans "invoice.settle"
  , State "settled"
  , Action "invoice.invoice_settled_action"
  ]

-- Deploy kronor:statechart/invoice_flow to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('invoice_flow', 0.1::semver) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
(chart, 'settling', 'wating for full settlement', null, true, false, null, null),
(chart, 'debt_collection', 'sent to debt collection', null, false, true, null, null),
(chart, 'transferred_to_account', 'transferred to account', null, false, true, null, null),
(chart, 'settled', 'settled', null, false, true, ('invoice', 'settled_action'), null),
(chart, 'in_progress', 'waiting for minimum payment', 'settling', true, false, null, null),
(chart, 'minimum_payment', 'received minimum payment', 'settling', false, false, null, null),
(chart, 'created', 'created', 'in_progress', true, false, ('invoice', 'created_action'), null),
(chart, 'due_date', 'due date', 'in_progress', false, false, ('invoice', 'due_action'), null),
(chart, 'reminder1', 'first reminder date', 'in_progress', false, false, ('invoice', 'reminder1_action'), null),
(chart, 'reminder2', 'second reminder date', 'in_progress', false, false, ('invoice', 'reminder2_action'), null),
(chart, 'debt_collection_date', 'debt collection date', 'in_progress', false, true, ('invoice', 'debt_collection_action'), null),
(chart, 'paid_more_than_min', 'paid enough to transfer to account', 'minimum_payment', true, false, ('invoice', 'paid_more_than_min_action'), null),
(chart, 'transferring_to_account', 'starting the transfer to account', 'minimum_payment', false, true, ('invoice', 'transferring_to_account_action'), null);
insert into fsm.transition (statechart_id, event, source_state, target_state) values
(chart, 'done.state.debt_collection_date', 'settling', 'debt_collection'),
(chart, 'done.state.minimum_payment', 'settling', 'transferred_to_account'),
(chart, 'invoice.settle', 'settling', 'settled'),
(chart, 'invoice.pay.enough', 'in_progress', 'minimum_payment'),
(chart, 'invoice.time.due', 'created', 'due_date'),
(chart, 'invoice.time.reminder1', 'due_date', 'reminder1'),
(chart, 'invoice.time.reminder2', 'reminder1', 'reminder2'),
(chart, 'invoice.time.debt_collection', 'reminder2', 'debt_collection_date'),
(chart, 'invoice.time.due', 'paid_more_than_min', 'transferring_to_account'),
(chart, 'invoice.time.past_due', 'paid_more_than_min', 'transferring_to_account');
end
$$;
COMMIT;

-- Deploy kronor:statechart/chart3 to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('chart3', 0.1::semver) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
(chart, 'waiting_for_capture', 'waiting for capture', null, true, false, ('runtime', 'check_for_capture'), null),
(chart, 'initializing', 'initialize payment', null, false, false, ('runtime', 'enqueue_payment_create_job'), null),
(chart, 'waiting_for_payment', 'waiting for payment', null, false, false, ('runtime', 'enqueue_payment_polling_job'), null),
(chart, 'paid', 'paid', null, false, true, ('runtime', 'promote_payment_to_marketplace'), null),
(chart, 'error', 'error', null, false, true, null, null),
(chart, 'declined', 'payment declined', null, false, true, null, null),
(chart, 'cancelled', 'payment cancelled', null, false, true, null, null),
(chart, 'processing', 'processing the payment', 'waiting_for_payment', true, false, null, null),
(chart, 'cancelling', 'cancel payment request', 'waiting_for_payment', false, false, ('runtime', 'enqueue_payment_cancel_job'), null);
insert into fsm.transition (statechart_id, event, source_state, target_state) values
(chart, 'payment.initialize', 'waiting_for_capture', 'initializing'),
(chart, 'payment.created', 'initializing', 'waiting_for_payment'),
(chart, 'payment.cancel', 'initializing', 'cancelled'),
(chart, 'payment.error', 'initializing', 'error'),
(chart, 'payment.paid', 'waiting_for_payment', 'paid'),
(chart, 'payment.declined', 'waiting_for_payment', 'declined'),
(chart, 'payment.cancelled', 'waiting_for_payment', 'cancelled'),
(chart, 'payment.error', 'waiting_for_payment', 'error'),
(chart, 'payment.cancel', 'processing', 'cancelling'),
(chart, 'payment.cancel.failed', 'cancelling', 'processing');
end
$$;
COMMIT;

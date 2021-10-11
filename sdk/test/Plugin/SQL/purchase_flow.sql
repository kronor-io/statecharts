-- Deploy kronor:statechart/purchase_flow to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('purchase_flow', 0.1::semver) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
(chart, 'assessing', 'risk assessing', null, true, false, null, null),
(chart, 'waiting_for_capture', 'waiting for capture', null, false, false, null, null),
(chart, 'payment_or_return', 'Waiting for full payment or full return', null, false, false, null, null),
(chart, 'cancelled', 'Order cancelled', null, false, true, null, null),
(chart, 'pre_flight_scoring', 'pre flight assessment', 'assessing', true, false, null, null),
(chart, 'sca_required', 'authenticating customer', 'assessing', false, false, null, null),
(chart, 'risk_scoring', 'risk scoring customer', 'assessing', false, true, ('dna', 'enqueue_job_fetch_uc_report'), null),
(chart, 'waiting', 'waiting for capture', 'waiting_for_capture', true, false, ('checkout', 'promote_purchase_order'), null),
(chart, 'cancelled_partially', 'partially cancelled', 'waiting_for_capture', false, false, null, null),
(chart, 'captured_partially', 'captured partially', 'waiting_for_capture', false, false, null, null),
(chart, 'cancelled_fully', 'fully cancelled', 'waiting_for_capture', false, true, null, null),
(chart, 'captured_fully', 'captured fully', 'waiting_for_capture', false, true, null, null),
(chart, 'waiting_for_payment', 'Waiting for payment', 'payment_or_return', true, false, null, null),
(chart, 'paid', 'Paid', 'payment_or_return', false, false, null, null),
(chart, 'returned_after_paid', 'Paid with returned items', 'payment_or_return', false, false, null, null),
(chart, 'fully_returned', 'Fully returned', 'payment_or_return', false, true, null, null),
(chart, 'done', 'All done', 'payment_or_return', false, true, null, null);
insert into fsm.transition (statechart_id, event, source_state, target_state) values
(chart, 'order.cancelled.manually', 'assessing', 'cancelled'),
(chart, 'assessment.approved', 'assessing', 'waiting_for_capture'),
(chart, 'preflight.approved', 'pre_flight_scoring', 'sca_required'),
(chart, 'auth.approved', 'sca_required', 'risk_scoring'),
(chart, 'activated', 'waiting_for_capture', 'payment_or_return'),
(chart, 'purchase.capture.partially', 'waiting', 'captured_partially'),
(chart, 'purchase.capture.fully', 'waiting', 'captured_fully'),
(chart, 'purchase.cancelled.manually', 'waiting', 'cancelled_fully'),
(chart, 'purchase.capture.fully', 'captured_partially', 'captured_fully'),
(chart, 'purchase.cancelled.manually', 'captured_partially', 'cancelled_partially'),
(chart, 'fully_returned', 'waiting_for_payment', 'fully_returned'),
(chart, 'invoice.paid', 'waiting_for_payment', 'paid'),
(chart, 'fully_returned', 'paid', 'fully_returned'),
(chart, 'return_policy_over', 'paid', 'done'),
(chart, 'returned.partial', 'paid', 'returned_after_paid'),
(chart, 'fully_returned', 'returned_after_paid', 'fully_returned'),
(chart, 'return_policy_over', 'returned_after_paid', 'done'),
(chart, 'returned.fully', 'returned_after_paid', 'fully_returned'),
(chart, 'return_policy.timeout', 'returned_after_paid', 'done');
end
$$;
COMMIT;

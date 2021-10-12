var scxml = [
{name: 'purchase_flow.scxml', data: ` <scxml xmlns="http://www.w3.org/2005/07/scxml"
              version="1.0"
              initial="assessing">
  <state id="assessing" name="risk assessing">
    <transition event="order.cancelled.manually" target="cancelled"/>
    <transition event="assessment.approved" target="waiting_for_capture"/>
    <initial>
      <transition target="pre_flight_scoring"/>
    </initial>
    <state id="pre_flight_scoring" name="pre flight assessment">
      <transition event="preflight.approved" target="sca_required"/>
    </state>
    <state id="sca_required" name="authenticating customer">
      <transition event="auth.approved" target="risk_scoring"/>
      <onentry>
        <script src="dna.check_sca_requirement" />
      </onentry>
    </state>
    <final id="risk_scoring" name="risk scoring customer">
      <onentry>
        <script src="dna.enqueue_job_fetch_uc_report" />
      </onentry>
    </final>
  </state>
  <state id="waiting_for_capture" name="waiting for capture">
    <transition event="activated" target="payment_or_return"/>
    <initial>
      <transition target="waiting"/>
    </initial>
    <state id="waiting" name="waiting for capture">
      <onentry>
        <script src="checkout.promote_purchase_order" />
      </onentry>
      <transition event="purchase.capture.partially" target="captured_partially"/>
      <transition event="purchase.capture.fully" target="captured_fully"/>
      <transition event="purchase.cancelled.manually" target="cancelled_fully"/>
    </state>
    <state id="cancelled_partially" name="partially cancelled">
    </state>
    <state id="captured_partially" name="captured partially">
      <transition event="purchase.capture.fully" target="captured_fully"/>
      <transition event="purchase.cancelled.manually" target="cancelled_partially"/>
    </state>
    <final id="cancelled_fully" name="fully cancelled">
    </final>
    <final id="captured_fully" name="captured fully">
    </final>
  </state>
  <state id="payment_or_return" name="Waiting for full payment or full return">
    <initial>
      <transition target="waiting_for_payment"/>
    </initial>
    <state id="waiting_for_payment" name="Waiting for payment">
      <transition event="fully_returned" target="fully_returned"/>
      <transition event="invoice.paid" target="paid"/>
    </state>
    <state id="paid" name="Paid">
      <transition event="fully_returned" target="fully_returned"/>
      <transition event="return_policy_over" target="done"/>
      <transition event="returned.partial" target="returned_after_paid"/>
    </state>
    <state id="returned_after_paid" name="Paid with returned items">
      <transition event="fully_returned" target="fully_returned"/>
      <transition event="return_policy_over" target="done"/>
      <transition event="returned.fully" target="fully_returned"/>
      <transition event="return_policy.timeout" target="done"/>
    </state>
    <final id="fully_returned" name="Fully returned">
    </final>
    <final id="done" name="All done">
    </final>
  </state>
  <final id="cancelled" name="Order cancelled">
  </final>
</scxml>
`},
{name: 'invoice_flow.scxml', data: `<scxml xmlns="http://www.w3.org/2005/07/scxml"
              version="1.0"
              initial="settling">
    <state id="settling" name="wating for full settlement">
        <initial>
            <transition target="in_progress"/>
        </initial>
        <state id="in_progress" name="waiting for minimum payment">
            <initial>
                <transition target="created"/>
            </initial>
            <state id="created" name="created">
                <transition event="invoice.time.due" target="due_date"/>
                <transition event="invoice.time.soft_reminder" target="in_progress_soft_reminder"/>
                <onentry>
                    <script src="invoice.created_action" />
                </onentry>
            </state>
            <state id="due_date" name="due date">
                <transition event="invoice.time.reminder1" target="reminder1"/>
                <onentry>
                    <script src="invoice.due_action" />
                </onentry>
            </state>
            <state id="reminder1" name="first reminder date">
                <transition event="invoice.time.reminder2" target="reminder2"/>
                <onentry>
                    <script src="invoice.reminder1_action" />
                </onentry>
            </state>
            <state id="reminder2" name="second reminder date">
                <transition event="invoice.time.debt_collection" target="debt_collection_date"/>
                <onentry>
                    <script src="invoice.reminder2_action" />
                </onentry>
            </state>
            <final id="debt_collection_date" name="debt collection date">
                <onentry>
                    <script src="invoice.debt_collection_action" />
                </onentry>
            </final>
            <state id="in_progress_soft_reminder" name="reminder before due date">
                <transition event="invoice.time.due" target="due_date"/>
                <onentry>
                    <script src="invoice.soft_reminder_action" />
                </onentry>
            </state>
            <transition event="invoice.pay.enough" target="minimum_payment"/>
        </state>
        <state id="minimum_payment" name="received minimum payment">
            <initial>
                <transition target="paid_more_than_min"/>
            </initial>
            <state id="paid_more_than_min" name="paid enough to transfer to account">
                <onentry>
                    <script src="invoice.paid_more_than_min_action" />
                </onentry>
                <transition event="invoice.time.soft_reminder" target="minimum_payment_soft_reminder"/>
                <transition event="invoice.time.due" target="transferring_to_account"/>
                <transition event="invoice.time.past_due" target="transferring_to_account"/>
            </state>
            <state id="minimum_payment_soft_reminder" name="reminder before due date">
                <transition event="invoice.time.due" target="transferring_to_account"/>
                <onentry>
                    <script src="invoice.soft_reminder_action" />
                </onentry>
            </state>
            <final id="transferring_to_account" name="starting the transfer to account">
                <onentry>
                    <script src="invoice.transferring_to_account_action" />
                </onentry>
            </final>
        </state>
        <transition event="done.state.debt_collection_date" target="debt_collection"/>
        <transition event="done.state.minimum_payment" target="transferred_to_account"/>
        <transition event="invoice.settle" target="settled"/>
    </state>
    <final id="debt_collection" name="sent to debt collection">
    </final>
    <final id="transferred_to_account" name="transferred to account">
    </final>
    <final id="settled" name="settled">
        <onentry>
            <script src="invoice.settled_action" />
        </onentry>
    </final>
</scxml>
`},
{name: 'payment_flow.scxml', data: `<scxml xmlns="http://www.w3.org/2005/07/scxml"
              version="1.0"
              initial="waiting_for_capture">
  <state id="waiting_for_capture" name="waiting for capture">
    <transition event="payment.initialize" target="initializing"/>
    <onentry>
      <script src="runtime.check_for_capture" />
    </onentry>
  </state>
  <state id="initializing" name="initialize payment">
    <transition event="payment.created" target="waiting_for_payment"/>
    <transition event="payment.cancel" target="cancelled"/>
    <transition event="payment.error" target="error"/>
    <onentry>
      <script src="runtime.enqueue_payment_create_job" />
    </onentry>
  </state>
  <state id="waiting_for_payment" name="waiting for payment">
    <transition event="payment.paid" target="paid"/>
    <transition event="payment.declined" target="declined"/>
    <transition event="payment.cancelled" target="cancelled"/>
    <transition event="payment.error" target="error"/>
    <onentry>
      <script src="runtime.enqueue_payment_polling_job" />
    </onentry>
    <initial>
      <transition target="processing"/>
    </initial>
    <state id="processing" name="processing the payment">
    <transition event="payment.cancel" target="cancelling"/>
    </state>
    <state id="cancelling" name="cancel payment request">
      <onentry>
        <script src="runtime.enqueue_payment_cancel_job" />
      </onentry>
      <transition event="payment.cancel.failed" target="processing"/>
    </state>
  </state>

  <final id="paid" name="paid">
    <onentry>
      <script src="runtime.promote_payment_to_marketplace" />
    </onentry>
  </final>

  <final id="error" name="error">
  </final>

  <final id="declined" name="payment declined">
  </final>

  <final id="cancelled" name="payment cancelled">
  </final>

</scxml>
`}
];
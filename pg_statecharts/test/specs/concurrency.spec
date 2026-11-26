# Two transactions sending events to the same machine at the same time.
#
# Handling an event reads the machine's active states and then exits and
# enters states based on what it read, and nothing holds those two together.
# Both transactions below read the machine as `open`, and the one that loses
# the race is acting on a configuration the machine has already left.
#
# Before 0.2.1 it applied that transition anyway and the machine ended up in
# `paid` and `voided` at once, both final, with both on_entry callbacks run.
# Now exiting the source states checks that they were all still active, so the
# loser raises instead. Send events at repeatable read or serializable, and
# retry 40001; this is the backstop for when something does not.

setup
{
  set client_min_messages to warning;
  create extension if not exists pg_statecharts cascade;
  reset client_min_messages;

  -- Which machine the steps below send to. A table because each session is
  -- its own connection and cannot be handed a value any other way.
  create table invoice (machine_id bigint not null);

  -- Records every callback that runs, so that the test can show the machine
  -- did not merely end up in one state but never ran the other's entry.
  create table invoice_entered (state_id text not null);

  create function entered(event_payload fsm_event_payload) returns void as
  $$
    insert into invoice_entered (state_id) values ((event_payload).to_state)
  $$ language sql;

  do $$
  declare
    chart bigint;
  begin
    insert into fsm.statechart (name, version)
    values ('invoice_flow', fsm.to_semver('1.0.0'))
    returning id into chart;

    insert into fsm.state (statechart_id, id, name, is_initial, is_final, on_entry) values
      (chart, 'open',   'OPEN',   true,  false, array[]::fsm_callback_name[]),
      (chart, 'paid',   'PAID',   false, true,  array[('public', 'entered')]::fsm_callback_name[]),
      (chart, 'voided', 'VOIDED', false, true,  array[('public', 'entered')]::fsm_callback_name[]);

    insert into fsm.transition (statechart_id, event, source_state, target_state) values
      (chart, 'invoice.pay',  'open', 'paid'),
      (chart, 'invoice.void', 'open', 'voided');

    insert into invoice (machine_id)
    select id from fsm.start_machine_with_latest_statechart(statechart_name => 'invoice_flow');
  end $$;
}

teardown
{
  drop table invoice;
  drop table invoice_entered;
  drop function entered(fsm_event_payload);
  delete from fsm.statechart;
}

session "s1"
step "s1_read_committed"  { begin transaction isolation level read committed; }
step "s1_repeatable_read" { begin transaction isolation level repeatable read; }
step "s1_pay"             { select fsm.notify_state_machine(machine_id => (select machine_id from invoice), event_name => 'invoice.pay'); }
step "s1_commit"          { commit; }

session "s2"
step "s2_read_committed"  { begin transaction isolation level read committed; }
step "s2_repeatable_read" { begin transaction isolation level repeatable read; }
step "s2_void"            { select fsm.notify_state_machine(machine_id => (select machine_id from invoice), event_name => 'invoice.void'); }
step "s2_commit"          { commit; }

session "observer"
step "active" {
  select s.state_id
  from fsm.state_machine_state s
  where s.exited_at is null
  order by s.state_id;
}
step "entered" { select state_id from invoice_entered order by state_id; }

# Read committed. s2 waits for s1 at the row lock on the state it is exiting,
# and when it is let through it finds that state already exited: its snapshot
# said `open` was active and it no longer is. It raises rather than applying
# `invoice.void` to a machine that has moved past it. One final state, one
# on_entry callback, and the event is not lost quietly -- the transaction that
# sent it fails and can retry.
permutation "s1_read_committed" "s2_read_committed" "s1_pay" "s2_void" "s1_commit" "s2_commit" "active" "entered"

# Repeatable read, which is what the README asks for. PostgreSQL itself
# refuses the write, before the check above has anything to say, and the error
# is its own rather than ours.
permutation "s1_repeatable_read" "s2_repeatable_read" "s1_pay" "s2_void" "s1_commit" "s2_commit" "active" "entered"

# Uncontended, for contrast: nothing waits, nothing raises, and the second
# event is handled against the state the first one left behind -- `invoice.void`
# no longer applies to a machine in `paid`, so it is dropped.
permutation "s1_read_committed" "s1_pay" "s1_commit" "s2_read_committed" "s2_void" "s2_commit" "active" "entered"

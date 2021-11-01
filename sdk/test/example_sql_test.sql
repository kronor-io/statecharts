
BEGIN;
-- the general flow, assuming the simplest version
-- is that we first we initiate a new a machine.
-- after that we check in which state this machine is
-- and if it is in the expected stated.
-- then we start testing the paths individually
-- by using notify_state_machine and then checking in which state are we, and in case there was actions to be handled, if the actions that we intercepeted are the same ones. this mostly still only tests the state chart machinery.
-- when we extend the scxml then we will be able to test stuff that are relevant to us, lets call it business logic per se instead of just testing the state chart machinery.

-- TODO idea, create a separate table for when the test are to run so we can do inserts from inside the intercepeted handle_machine_events fn.

-- TODO alternatively we may pass the is or ok fn inside the body of the intercepeted fn. will it work?

create or replace table intercepted ...
  -- TODO
  -- event
  -- message
  -- when
create or replace function last_intercepted()
  -- TODO
  -- select by sorting by time

-- NOTE: this function only needs to be replaced once
create or replace function fsm.handle_machine_events()....
 $$
  insert into intercepted () values .....
 $$

-- NOTE: the first cluster will never have a transition, its just a initial check of state plus the action test
 --select notify_state_machine(...);
 select notify_state_machine(1,:mid, '', ''::jsonb);
 with foo as foo (select last_intercepted()) select is(:foo,'invoice.created_action');

 select notify_state_machine(1,:mid, '', ''::jsonb);
 select fsm.state_machine_state where ...;
 with foo as foo (select last_intercepted()) select is(:foo,'invoice.soft_reminder_action');

 select notify_state_machine(1,:mid, '', ''::jsonb);
 select fsm.state_machine_state where ...;
 with foo as foo (select last_intercepted()) select is(:foo,'invoice.due_date_action');

-- TODO and by last consider how we will add the extra conditions when extending the scxml?
-- they will be checked from inside the intercepted function or outside? and if from inside the intercepted function, we cannot defer the event, we need to incercept and maybe pass it along so it can has it effect? not sure.

  -- create the machine of name invoice_flow with lastest chart
  -- run first cluster, meaning do the transition, check the state, check the action

ROLLBACK;

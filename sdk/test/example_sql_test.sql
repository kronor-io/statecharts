
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
create or replace function last_intercepted()....

create or replace function fsm.handle_machine_events()....
 $$
  insert into intercepted () values .....
 $$

 --select notify_state_machine(...);
 select notify_state_machine(1,:mid, '', ''::jsonb);
 with foo as foo (select last_intercepted()) select is(:foo,'invoice.created_action');

 select notify_state_machine(1,:mid, '', ''::jsonb);
 select fsm.state_machine_state where ...;
 with foo as foo (select last_intercepted()) select is(:foo,'invoice.soft_reminder_action');

 select notify_state_machine(1,:mid, '', ''::jsonb);
 select fsm.state_machine_state where ...;
 with foo as foo (select last_intercepted()) select is(:foo,'invoice.due_date_action');

 -- etc

  -- create the machine of name invoice_flow with lastest chart
  -- run first cluster, meaning do the transition, check the state, check the action

ROLLBACK;

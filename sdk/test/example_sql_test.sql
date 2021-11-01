-- manually written, but mimicking the generated code, will be here
BEGIN;
-- the general flow, assuming the simplest version
-- is that we first we initiate a new a machine.
-- after that we check in which state this machine is
-- and if it is in the expected stated.
-- then we start testing the paths individually
-- by using notify_state_machine and then checking in which state are we, and in case there was actions to be handled, if the actions that we intercepeted are the same ones. this mostly still only tests the state chart machinery.
-- when we extend the scxml then we will be able to test stuff that are relevant to us, lets call it business logic per se instead of just testing the state chart machinery.
ROLLBACK;

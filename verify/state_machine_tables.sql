-- Verify statecharts:state_machine_tables on pg

BEGIN;

 select * from fsm.state_machine_event;
 select * from fsm.state_machine_state;
 select * from fsm.state_machine;

ROLLBACK;

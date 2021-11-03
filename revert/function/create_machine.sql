-- Revert statecharts:function/create_machine from pg

BEGIN;

  drop function if exists fsm.create_machine(bigint, bigint);

COMMIT;

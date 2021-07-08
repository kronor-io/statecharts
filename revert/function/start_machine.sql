-- Revert statecharts:function/start_machine from pg

BEGIN;

  drop function if exists fsm.start_machine(bigint, bigint, jsonb, bigint);

COMMIT;

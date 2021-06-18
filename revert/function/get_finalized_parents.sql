-- Revert statecharts:function/get_finalized_parents from pg

BEGIN;

  drop function if exists fsm.get_finalized_parents(bigint, bigint, ltree);

COMMIT;

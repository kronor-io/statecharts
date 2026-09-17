-- Revert example:lightswitch from pg

BEGIN;

  drop table lightswitch;

COMMIT;

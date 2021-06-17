-- Verify statecharts:trigger/check_no_cross_boundary_transitions on pg

BEGIN;

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_no_cross_boundary_transitions_insert'
    and event_object_schema = 'fsm'
    and event_object_table = 'transition';

  select 1 / count(*)
  from information_schema.triggers
  where trigger_name = 'check_no_cross_boundary_transitions_update'
    and event_object_schema = 'fsm'
    and event_object_table = 'transition';

ROLLBACK;

-- Intermediate representation of a parsed .scxml document, sitting between the
-- XML and the fsm.state / fsm.transition tables. Both import_scxml_files and
-- gen_statechart_sqitch_migrations work off these, so the two can never
-- disagree about what a chart means.

create type fsm.scxml_state as (
  id text,
  name text,
  parent_id text,
  is_initial boolean,
  is_final boolean,
  on_entry fsm_callback_name[],
  on_exit fsm_callback_name[]
);

create type fsm.scxml_transition as (
  event text,
  source_state text,
  target_state text
);

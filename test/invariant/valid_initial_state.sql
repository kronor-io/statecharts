BEGIN;
  select plan(9);

  insert into fsm.statechart (id, name, version)
  values (9000000000,'test', 1::semver);

  prepare insert_state as
  insert into fsm.state (statechart_id, id, name, is_initial, is_final)
  values (9000000000, $1, $1, $2, false);

  prepare insert_compound as
  insert into fsm.compound_state (statechart_id, parent_state, child_state)
  values (9000000000, $1, $2);

  set constraints all immediate;

  select lives_ok(
    $$execute insert_state('my_initial', true)$$,
    'inserting an initial should work'
  );

  select throws_like(
    $$execute insert_state('another_initial', true)$$,
    'exactly one initial state is required per statechart',
    'initial states should be globally unique'
  );

  select lives_ok(
    $$execute insert_state('second', false)$$,
    'inserting a non-initial state should work'
  );

  -- now insert some nested states, notices that in compound states,
  -- many children can be the initial state at the same time. That is
  -- the case for parallel states. We need to defer the constraints again
  -- so we can insert in the two tables without an error.
  set constraints all deferred;

  execute insert_state('third', true);
  execute insert_state('fourth', true);
  execute insert_state('another', false);
  execute insert_compound('my_initial', 'second');
  execute insert_compound('second', 'third');
  execute insert_compound('second', 'fourth');
  execute insert_compound('fourth', 'another');

  -- now let's trigger the check again by inserting a new state and make sure
  -- that we are still in a consistent state
  set constraints all immediate;
  select lives_ok(
    $$execute insert_state('fifth', false)$$,
    'check again that we are in a consistent state'
  );

  select throws_like(
    $$update fsm.state set is_initial = true where id = 'fifth' and statechart_id = 9000000000$$,
    'exactly one initial state is required per statechart',
    'updates are also verified'
  );

  select lives_ok(
    $$delete from fsm.state where id = 'fifth' and statechart_id = 9000000000$$,
    'deleting non-initial states is ok'
  );

  select throws_like(
    $$delete from fsm.state where id = 'my_initial' and statechart_id = 9000000000$$,
    'exactly one initial state is required per statechart',
    'deletes are also verified'
  );

  select throws_like(
    $$execute insert_compound('another', 'my_initial')$$,
    'exactly one initial state is required per statechart',
    'check when the global initial is made a child state'
  );

  select throws_like(
    $$update fsm.compound_state set child_state = 'my_initial' where parent_state = 'fourth'$$,
    'exactly one initial state is required per statechart',
    'check when the global initial is made a child state'
  );

  select finish();
ROLLBACK;

BEGIN;
  select plan(2);

  insert into fsm.statechart (id, name, version) values (9000000000,'test', 1::semver);

  prepare insert_state as
  insert into fsm.state (statechart_id, id, name, is_initial, is_final)
  values (9000000000, $1, $1, $2, false);

  prepare insert_compound as
  insert into fsm.compound_state (statechart_id, parent_state, child_state)
  values (9000000000, $1, $2);

  execute insert_state('my_initial', true);
  select is('my_initial', id, 'my_initial should be the only initial state')
  from fsm.get_initial_state(9000000000);


  execute insert_state('second', false);
  execute insert_state('third', false);
  execute insert_state('fourth', false);

  execute insert_compound('my_initial', 'second');
  execute insert_compound('my_initial', 'third');
  execute insert_compound('third', 'fourth');

  select is('my_initial', id, 'my_initial should be the only initial state')
  from fsm.get_initial_state(9000000000);

  select finish();
ROLLBACK;

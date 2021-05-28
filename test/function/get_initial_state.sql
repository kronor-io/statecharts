BEGIN;
  select plan(2);

  insert into fsm.statechart (id, name, version) values (9000000000,'test', 1::semver);

  prepare insert_state as
  insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final)
  values (9000000000, $1, $1, $2, $3, false);

  execute insert_state('my_initial', null, true);
  select is('my_initial', id, 'my_initial should be the only initial state')
  from fsm.get_initial_state(9000000000);


  execute insert_state('second' , 'my_initial' , false);
  execute insert_state('third'  , 'my_initial' , false);
  execute insert_state('fourth' , 'third'      , false);

  select is('my_initial', id, 'my_initial should be the only initial state')
  from fsm.get_initial_state(9000000000);

  select finish();
ROLLBACK;

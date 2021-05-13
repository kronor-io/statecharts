BEGIN;

  select no_plan();

  set constraints all immediate;

  insert into fsm.statechart (id , name , version) values (1 , 'test' , 1::semver);
  insert into fsm.state (statechart_id, id, name , is_initial, is_final)
  values
    (1, 'd', 'd', true, false);

  insert into fsm.state (statechart_id, id, name , is_initial, is_final)
  values
    (1, 'b', 'b', false, false),
    (1, 'c', 'c', false, false),
    (1, 'a', 'a', false, false);

  prepare insert_compound as
  insert into fsm.compound_state (statechart_id, parent_state, child_state)
  values
  (1, 'a' , 'b'),
  (1, 'a' , 'c'),
  (1, 'd' , 'a'),
  (1, 'c' , 'b');

  select lives_ok('insert_compound', 'should fail with multiple parents');

    select child_state
    from fsm.compound_state
    where statechart_id = 1
    group by child_state
    having count(*) > 1;



  select finish();

ROLLBACK;

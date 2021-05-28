BEGIN;
  select no_plan();

  insert into fsm.statechart (id, name, version)
  values (9000000000,'test', 1::semver);

  set constraints all immediate;

  prepare test_input as
  insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final)
  values
    (9000000000 , 'initial' , 'initial' , null , true  , false),
    (9000000000 , 'final'   , 'final'   , null , false , true);

  select lives_ok('test_input', 'inserting an initial and final state should work');

  prepare bad_input as
  insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final)
  values
    (9000000000 , 'another_final' , 'another_final' , null , false  , true);

  select throws_like(
    'bad_input',
    '%final_state_should_be_unique%',
    'two top level final states are bad'
  );

  prepare nested_input as
  insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final)
  values
    (9000000000 , 'nested'       , 'nested' , 'initial' , true  , false) ,
    (9000000000 , 'final_nested' , 'final'  , 'initial' , false , true);

  select lives_ok('nested_input', 'compound states can also have a final state');

  prepare bad_nested_input as
  insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final)
  values
    (9000000000 , 'second_final_nested' , 'second final'  , 'initial' , false , true);

  select throws_like(
    'bad_nested_input',
    '%final_state_should_be_unique%',
    'two  same-level final states are bad'
  );

  select finish();
ROLLBACK;

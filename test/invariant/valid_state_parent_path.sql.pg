BEGIN;
  select plan(10);

  insert into fsm.statechart (id, name, version)
  values (9000000000,'test', 1::semver);

  prepare insert_state as
  insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final)
  values (9000000000, $1, $1, $2, $3, false);

  execute insert_state('my_initial'   , null         , true);
  execute insert_state('another_root' , null         , false);
  execute insert_state('second'       , 'my_initial' , false);
  execute insert_state('third'        , 'second'     , false);
  execute insert_state('fourth'       , 'second'     , true);
  execute insert_state('another'      , 'fourth'     , false);

  select is(parent_path, '9000000000'::ltree, 'roots should have the statechart id as path')
  from fsm.state
  where statechart_id = 9000000000
    and id = any(array['my_initial', 'another_root']);

  select is(node_path, '9000000000.my_initial'::ltree, 'full path should be set')
  from fsm.state
  where statechart_id = 9000000000
    and id = any(array['my_initial']);

  select is(node_path, '9000000000.another_root'::ltree, 'full path should be set')
  from fsm.state
  where statechart_id = 9000000000
    and id = any(array['another_root']);

  select is(parent_path, '9000000000.my_initial'::ltree, 'one level tree nesting')
  from fsm.state
  where statechart_id = 9000000000
    and id = any(array['second']);

  select is(parent_path, '9000000000.my_initial.second'::ltree, 'two level tree nesting')
  from fsm.state
  where statechart_id = 9000000000
    and id = any(array['third', 'fourth']);

  select is(parent_path, '9000000000.my_initial.second.fourth'::ltree, 'three level tree nesting')
  from fsm.state
  where statechart_id = 9000000000
    and id = any(array['another']);

  select is(node_path, '9000000000.my_initial.second.fourth.another'::ltree, 'three level tree nesting full path')
  from fsm.state
  where statechart_id = 9000000000
    and id = any(array['another']);


  update fsm.state set parent_id = 'second' where id = 'another' and statechart_id = 9000000000;

  select
      is(parent_path, '9000000000.my_initial.second'::ltree, 'updated parent_id')
    , is(node_path, '9000000000.my_initial.second.another'::ltree, 'updated parent_id full path')
  from fsm.state
  where statechart_id = 9000000000
    and id = any(array['another']);

  select finish();
ROLLBACK;

BEGIN;
  select plan(30);

  insert into fsm.statechart (id, name, version) values (9000000001,'search', 1::semver);

  insert into fsm.state
    (statechart_id , id           , name                 , parent_id , is_initial , is_final) values
    (9000000001    , 'initial'    , 'Initial'            , null      , true       , false)           ,
    (9000000001    , 'searching'  , 'Searching'          , null      , false      , false)           ,
    (9000000001    , 'displaying' , 'Displaying Results' , null      , false      , false)           ,
    (9000000001    , 'zoomed_in'  , 'Zoomed In'          , null      , false      , false);

  insert into fsm.transition
    (statechart_id , event      , source_state , target_state) values
    (9000000001    , 'search'   , 'initial'    , 'searching')         ,
    (9000000001    , 'results'  , 'searching'  , 'displaying')        ,
    (9000000001    , 'zoom'     , 'displaying' , 'zoomed_in')         ,
    (9000000001    , 'zoom_out' , 'zoomed_in'  , 'displaying');

  -- creating a new machine should be a function
  insert into fsm.state_machine
    (id          , statechart_id) values
    (80000000000 , 9000000001);

  insert into fsm.state_machine_state
    (state_machine_id, statechart_id, state_id) values
    (80000000000, 9000000001, 'initial');


  -- transition to the search state

  insert into fsm.state_machine_event
    (state_machine_id, name, data) values
    (80000000000, 'search', '{}');

  select lives_ok(
    $$ select fsm.handle_machine_events(80000000000) $$,
    'handled a single simple event'
  );

  select is(count(*), 0::bigint, 'all events should be handled')
  from fsm.state_machine_event
  where state_machine_id = 80000000000 and handled_at is null;

  select is(exited_at, now(), 'should have exited the initial event')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and state_id = 'initial';

  select is(count(*), 1::bigint, 'there sould only be one active state')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  select is(state_id, 'searching', 'the active state should be searching')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  -- transition to the displaying state

  insert into fsm.state_machine_event
    (state_machine_id, name, data) values
    (80000000000, 'results', '{}');

  select lives_ok(
    $$ select fsm.handle_machine_events(80000000000) $$,
    'handled a single simple event'
  );

  select is(count(*), 0::bigint, 'all events should be handled')
  from fsm.state_machine_event
  where state_machine_id = 80000000000 and handled_at is null;

  select is(exited_at, now(), 'should have exited the search event')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and state_id = 'searching';

  select is(count(*), 1::bigint, 'there sould only be one active state')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  select is(state_id, 'displaying', 'the active state should be displaying')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  -- transition to the zoomed_in state

  insert into fsm.state_machine_event
    (state_machine_id, name, data) values
    (80000000000, 'zoom', '{}');

  select lives_ok(
    $$ select fsm.handle_machine_events(80000000000) $$,
    'handle the zoom event'
  );

  select is(count(*), 0::bigint, 'all events should be handled')
  from fsm.state_machine_event
  where state_machine_id = 80000000000 and handled_at is null;

  select is(exited_at, now(), 'should have exited the displaying event')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and state_id = 'displaying';

  select is(count(*), 1::bigint, 'there sould only be one active state')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  select is(state_id, 'zoomed_in', 'the active state should be displaying')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  -- transition back to displaying

  insert into fsm.state_machine_event
    (state_machine_id, name, data) values
    (80000000000, 'zoom_out', '{}');

  select lives_ok(
    $$ select fsm.handle_machine_events(80000000000) $$,
    'handle the zoom_out event'
  );

  select is(count(*), 0::bigint, 'all events should be handled')
  from fsm.state_machine_event
  where state_machine_id = 80000000000 and handled_at is null;

  select is(exited_at, now(), 'should have exited the displaying event')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and state_id = 'zoomed_in';

  select is(count(*), 1::bigint, 'there sould only be one active state')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  select is(state_id, 'displaying', 'the active state should be displaying')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;


  -- verify that events with no associated transitions are not an issue

  insert into fsm.state_machine_event
    (state_machine_id, name, data) values
    (80000000000, 'derp', '{}');

  select lives_ok(
    $$ select fsm.handle_machine_events(80000000000) $$,
    'handle the derp event'
  );

  select is(count(*), 0::bigint, 'all events should be handled')
  from fsm.state_machine_event
  where state_machine_id = 80000000000 and handled_at is null;

  select is(exited_at, now(), 'should have exited the zoomed_in event')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and state_id = 'zoomed_in';

  select is(count(*), 1::bigint, 'there sould only be one active state')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  select is(state_id, 'displaying', 'the active state should be displaying')
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;


  -- verify that multiple events can be handled at once as long as they come in order

  -- creating a new machine should be a function
  insert into fsm.state_machine
    (id          , statechart_id) values
    (80000000001 , 9000000001);

  insert into fsm.state_machine_state
    (state_machine_id, statechart_id, state_id) values
    (80000000001, 9000000001, 'initial');


  -- transition to the search state

  insert into fsm.state_machine_event
    (state_machine_id, name, data) values
    (80000000001, 'search', '{}'),
    (80000000001, 'results', '{}'),
    (80000000001, 'zoom', '{}');

  select lives_ok(
    $$ select fsm.handle_machine_events(80000000001) $$,
    'handle all the events'
  );

  select is(count(*), 0::bigint, 'all events should be handled')
  from fsm.state_machine_event
  where state_machine_id = 80000000001 and handled_at is null;

  select is(exited_at, now(), 'should have exited the displaying event')
  from fsm.state_machine_state
  where state_machine_id = 80000000001 and state_id = 'displaying';

  select is(count(*), 1::bigint, 'there sould only be one active state')
  from fsm.state_machine_state
  where state_machine_id = 80000000001 and exited_at is null;

  select is(state_id, 'zoomed_in', 'the active state should be zoomed_in')
  from fsm.state_machine_state
  where state_machine_id = 80000000001 and exited_at is null;

  select finish();
ROLLBACK;

BEGIN;
  select no_plan();

  insert into fsm.statechart (id, name, version) values (9000000001,'search', 1::semver);

  insert into fsm.state
    (statechart_id , id        , name      , parent_id , is_initial , is_final) values
    (9000000001    , 'initial' , 'Initial' , null      , true       , false)           ,

--  (statechart_id , id                , name              , parent_id   , is_initial , is_final) values
    (9000000001    , 'searching'       , 'Searching'       , null        , false      , false)           ,
    (9000000001    , 'googling'        , 'Googling'        , 'searching' , true       , false)           ,
    (9000000001    , 'checking_google' , 'Do the Googling' , 'googling'  , true       , false)           ,
    (9000000001    , 'got_google'      , 'Got Google'      , 'googling'  , false      , true)            ,
    (9000000001    , 'binging'         , 'Binging'         , 'searching' , true       , false)           ,
    (9000000001    , 'checking_bing'   , 'Do the Binging'  , 'binging'   , true       , false)           ,
    (9000000001    , 'got_bing'        , 'Got Bing'        , 'binging'   , false      , true)            ,

--  (statechart_id , id           , name                 , parent_id , is_initial , is_final)
    (9000000001    , 'displaying' , 'Displaying Results' , null      , false      , false);

  insert into fsm.transition
    (statechart_id , event               , source_state      , target_state) values
    (9000000001    , 'search'            , 'initial'         , 'searching')         ,
    (9000000001    , 'google_results'    , 'checking_google' , 'got_google')        ,
    (9000000001    , 'bing_results'      , 'checking_bing'   , 'got_bing')          ,
    (9000000001    , 'done.state.search' , 'searching'       , 'displaying');

  -- creating a new machine should be a function
  insert into fsm.state_machine
    (id          , statechart_id) values
    (80000000000 , 9000000001);

  insert into fsm.state_machine_state
    (state_machine_id, statechart_id, state_id) values
    (80000000000, 9000000001, 'initial');


  -- transition to the search state, which should activate all initial sub-states

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

  select is(
    array_agg(state_id)
  , array['binging', 'checking_bing', 'checking_google', 'googling', 'searching']
  , 'all initial states in the search tree should be active'
  )
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  -- Only get results from google now
  insert into fsm.state_machine_event
    (state_machine_id, name, data) values
    (80000000000, 'google_results', '{}');

  select lives_ok(
    $$ select fsm.handle_machine_events(80000000000) $$,
    'handle the google_results event'
  );

  select is(
    array_agg(state_id)
  , array['binging', 'checking_bing', 'googling', 'got_google', 'searching']
  , 'when a composite state activates its final state, the parent remains activated'
  )
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  select is(
      array_agg(name)
    , array['done.state.got_google', 'done.state.googling']
    , 'done.state.got_google and googling should be raised'
  )
  from fsm.state_machine_event
  where state_machine_id = 80000000000 and handled_at is null;

  -- Get results fro bing
  insert into fsm.state_machine_event
    (state_machine_id, name, data) values
    (80000000000, 'bing_results', '{}');

  select lives_ok(
    $$ select fsm.handle_machine_events(80000000000) $$,
    'handle the bing_results event'
  );

  select is(
    array_agg(state_id)
  , array['binging', 'googling', 'got_bing', 'got_google', 'searching']
  , 'The searching parallel state should have reached all final states'
  )
  from fsm.state_machine_state
  where state_machine_id = 80000000000 and exited_at is null;

  select is(
      array_agg(name)
    , array['done.state.got_bing', 'done.state.searching', 'done.state.binging']
    , 'donde events up to serching should be raised'
  )
  from fsm.state_machine_event
  where state_machine_id = 80000000000 and handled_at is null;


  select finish();
ROLLBACK;


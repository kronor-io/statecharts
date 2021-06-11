BEGIN;
  select plan(3);

  insert into fsm.statechart (id, name, version) values (9000000001,'search', 1::semver);

  insert into fsm.state
    (statechart_id , id         , name       , parent_id , is_initial , is_final) values
    (9000000001    , 'initial'  , 'Initial'  , null      , true       , false)           ,
    (9000000001    , 'research' , 'Research' , null      , false      , false)           ,

--  (statechart_id , id                , name              , parent_id   , is_initial , is_final) values
    (9000000001    , 'searching'       , 'Searching'       , 'research'  , true       , false)           ,
    (9000000001    , 'googling'        , 'Googling'        , 'searching' , true       , false)           ,
    (9000000001    , 'checking_google' , 'Do the Googling' , 'googling'  , true       , false)           ,
    (9000000001    , 'got_google'      , 'Got Google'      , 'googling'  , false      , true)            ,
    (9000000001    , 'binging'         , 'Binging'         , 'searching' , true       , false)           ,
    (9000000001    , 'checking_bing'   , 'Do the Binging'  , 'binging'   , true       , false)           ,
    (9000000001    , 'got_bing'        , 'Got Bing'        , 'binging'   , false      , true)            ,
    (9000000001    , 'read_results'    , 'Read'            , 'research'  , false      , true);

  insert into fsm.transition
    (statechart_id , event                  , source_state      , target_state) values
    (9000000001    , 'search'               , 'initial'         , 'research')          ,
    (9000000001    , 'google_results'       , 'checking_google' , 'got_google')        ,
    (9000000001    , 'bing_results'         , 'checking_google' , 'got_bing')          ,
    (9000000001    , 'done.state.searching' , 'searching'       , 'read_results');



  -- creating a new machine should be a function
  insert into fsm.state_machine
    (id          , statechart_id) values
    (80000000000 , 9000000001);

  insert into fsm.state_machine_state
    (state_machine_id, statechart_id, state_id) values
    (80000000000, 9000000001, 'searching'),
    (80000000000, 9000000001, 'googling'),
    (80000000000, 9000000001, 'checking_google'),
    (80000000000, 9000000001, 'binging'),
    (80000000000, 9000000001, 'checking_bing');

  select is(array_agg(p.id), null, 'when no final state has been reached it should return null')
  from fsm.get_finalized_parents(
      80000000000
    , (select parent_path from fsm.state where id = 'checking_google')
  ) p;

  update fsm.state_machine_state set exited_at = now()
    where state_machine_id = 80000000000
      and state_id = 'checking_google';

  insert into fsm.state_machine_state
    (state_machine_id, statechart_id, state_id) values
    (80000000000, 9000000001, 'got_google');

  select is(
      array_agg(p.id)
    , array['googling']
    , 'when only one parallel state finishes only the immediate parent should be present'
  )
  from fsm.get_finalized_parents(
      80000000000
    , (select parent_path from fsm.state where id = 'got_google')
  ) p;

  update fsm.state_machine_state set exited_at = now()
    where state_machine_id = 80000000000
      and state_id = 'checking_bing';

  insert into fsm.state_machine_state
    (state_machine_id, statechart_id, state_id) values
    (80000000000, 9000000001, 'got_bing');

  -- note that the results do not include the parent state "research", this is becuase
  -- there still is a possible transition going from searching to read_results inside thre
  -- "research" node. This means that we cannot consider "research" finalized.
  select is(
      array_agg(p.id)
    , array['searching', 'binging']
    , 'when all the final states for a parallel node are reached, the parent node is present in the result')
  from fsm.get_finalized_parents(
      80000000000
    , (select parent_path from fsm.state where id = 'got_bing')
  ) p;


  select finish();
ROLLBACK;

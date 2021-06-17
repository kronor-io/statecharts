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
    (9000000001    , 'bing_results'         , 'checking_bing'   , 'got_bing')          ,
    (9000000001    , 'done.state.searching' , 'searching'       , 'read_results');

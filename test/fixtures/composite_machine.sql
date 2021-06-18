  insert into fsm.statechart (id, name, version) values (:chart,'search', 1::semver);

  insert into fsm.state
    (statechart_id , id        , name      , parent_id , is_initial , is_final) values
    (:chart        , 'initial' , 'Initial' , null      , true       , false)           ,

--  (statechart_id , id                , name              , parent_id   , is_initial , is_final) values
    (:chart        , 'searching'       , 'Searching'       , null        , false      , false)           ,
    (:chart        , 'googling'        , 'Googling'        , 'searching' , true       , false)           ,
    (:chart        , 'checking_google' , 'Do the Googling' , 'googling'  , true       , false)           ,
    (:chart        , 'got_google'      , 'Got Google'      , 'googling'  , false      , true)            ,
    (:chart        , 'binging'         , 'Binging'         , 'searching' , true       , false)           ,
    (:chart        , 'checking_bing'   , 'Do the Binging'  , 'binging'   , true       , false)           ,
    (:chart        , 'got_bing'        , 'Got Bing'        , 'binging'   , false      , true)            ,

--  (statechart_id , id           , name                 , parent_id , is_initial , is_final)
    (:chart    , 'displaying' , 'Displaying Results' , null      , false      , false);

  insert into fsm.transition
    (statechart_id , event                  , source_state      , target_state) values
    (:chart        , 'search'               , 'initial'         , 'searching')         ,
    (:chart        , 'google_results'       , 'checking_google' , 'got_google')        ,
    (:chart        , 'bing_results'         , 'checking_bing'   , 'got_bing')          ,
    (:chart        , 'done.state.searching' , 'searching'       , 'displaying');

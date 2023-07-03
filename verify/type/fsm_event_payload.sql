-- Verify statecharts:type/fsm_event_payload on pg

BEGIN;

    select
      ( 1
      , 1
      , 'foo'
      , '{"bar": "baz"}'
      , 'from'
      , 'to'
      , 'type'
      )::fsm_event_payload;

ROLLBACK;

create type fsm_event_payload as
( shard_id bigint
, machine_id bigint
, event_name text
, data jsonb
, from_state text
, to_state text
, payload_type text
);

comment on type fsm_event_payload is
    'The argument passed to every on_entry and on_exit callback function.';

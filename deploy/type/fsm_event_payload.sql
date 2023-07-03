-- Deploy statecharts:type/fsm_event_payload to pg

BEGIN;

do $$
    begin
      create type fsm_event_payload as
      ( shard_id bigint
      , machine_id bigint
      , event_name text
      , data jsonb
      , from_state text
      , to_state text
      , payload_type text
      );
    exception when duplicate_object then null;
    end
$$;

COMMIT;

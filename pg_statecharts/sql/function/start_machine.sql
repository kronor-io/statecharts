-- See fsm.create_machine for what the trailing deprecated parameter is for.
--
-- A null machine_id returns null, standing in for the `strict` this function
-- never had. start_machine_with_latest_statechart depends on that: a chart
-- name matching nothing yields a null id, which has to come back as no rows.
--
-- A machine_id that names no machine is a different case and raises. There is
-- nothing to start, so returning null left the machine simply never running,
-- and the first complaint came from some unrelated call much later -- usually
-- a foreign key violation from notify_state_machine, pointing at the symptom
-- rather than at the call that was wrong.
create or replace function fsm.start_machine(
    machine_id   bigint default null
  , initial_data jsonb  default '{}'
  , shard_id     bigint default 1
  -- deprecated, removed in 0.3.0
  , shard        bigint default null
)
  returns fsm.state_machine as
  $$
    declare
      -- Resolved once. Every query below reads a shard_id column, so a bare
      -- shard_id would be ambiguous against the parameter of that name.
      machine_shard bigint := coalesce(shard, start_machine.shard_id);
      initial fsm.state%rowtype;
      machine fsm.state_machine%rowtype;
      error_message text;
      error_state text;
      error_context text;
      error_detail text;
      on_entry_ fsm_callback_name;
    begin

      select * into machine from fsm.state_machine sm where sm.shard_id = machine_shard and sm.id = start_machine.machine_id;

      if start_machine.machine_id is not null and machine.id is null then
        raise exception 'state machine with id % and shard_id % does not exist'
            , start_machine.machine_id, machine_shard
          using detail = 'The id and the shard_id together identify a machine,'
                         ' so a machine created under a different shard_id is'
                         ' not found by this one. Pass the shard_id the machine'
                         ' was created with, or omit the argument everywhere so'
                         ' that every call uses the default of 1.';
      end if;

      for initial in select * from fsm.get_initial_state(statechart_id => machine.statechart_id)
      loop

        insert into fsm.state_machine_state
          (shard_id, state_machine_id, statechart_id, state_id) values
          (machine_shard, machine.id, machine.statechart_id, initial.id);

        foreach on_entry_ in array initial.on_entry
        loop
            -- This call will look like something like this
            -- select my_schema.my_function(
            --  (1, 199, '__initial__', '{...}', null, 233, 'on_entry')::fsm.event_payload
            -- )
            execute format('select %I.%I(%L::fsm_event_payload)',
                coalesce((on_entry_).schema_name::text, 'public')
              , (on_entry_).function_name
              , ( machine_shard
                , machine.id
                , '__initial__'
                , initial_data
                , null
                , initial.id
                , 'on_entry'
                )::fsm_event_payload
            );
        end loop;

      end loop;

      return machine;

    exception when others then
      get stacked diagnostics
        error_message = message_text,
        error_context = pg_exception_context,
        error_detail = pg_exception_detail,
        error_state = returned_sqlstate;
      raise exception
        using errcode=error_state,
          message=error_message,
          detail = error_detail,
          hint=format('Error while handling state machine event: %s', error_context);
    end
  $$ language plpgsql volatile;

comment on function fsm.start_machine(bigint, jsonb, bigint, bigint) is $comment$
    Starts a given state machine by activating all of its initial states and
    executing the corresponding on_entry callbacks.
$comment$;

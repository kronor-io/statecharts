-- Deploy statecharts:function/start_machine to pg

BEGIN;

  create or replace function fsm.start_machine(shard bigint, machine_id bigint, initial_data jsonb default '{}')
    returns fsm.state_machine as
    $$
      declare
        initial fsm.state%rowtype;
        machine fsm.state_machine%rowtype;
        error_message text;
        error_state text;
        error_context text;
        error_detail text;
      begin

        select * into machine from fsm.state_machine where shard_id = shard and id = machine_id;

        for initial in select * from fsm.get_initial_state(machine.statechart_id)
        loop

          insert into fsm.state_machine_state
            (shard_id, state_machine_id, statechart_id, state_id) values
            (shard, machine.id, machine.statechart_id, initial.id);

          if initial.on_entry is not null
          then
              -- This call will look like something like this
              -- select my_schema.my_function(
              --  (1, 199, '__initial__', '{...}', null, 233, 'on_entry')::fsm.event_payload
              -- )
              execute format('select %I.%I(%L::fsm_event_payload)',
                  coalesce((initial.on_entry).schema_name, 'public')
                , (initial.on_entry).function_name
                , ( shard
                  , machine.id
                  , '__initial__'
                  , initial_data
                  , null
                  , initial.id
                  , 'on_entry'
                  )::fsm_event_payload
              );
          end if;

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

    comment on function fsm.start_machine(bigint, bigint, jsonb) is $comment$
        Starts a given state machine by activating all of its initial states and
        executing the corresponding on_entry callbacks.
    $comment$;

COMMIT;

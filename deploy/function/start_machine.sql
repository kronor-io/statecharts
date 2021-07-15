-- Deploy statecharts:function/start_machine to pg

BEGIN;

  create or replace function fsm.start_machine(shard bigint, statechart bigint, initial_data jsonb default '{}', machine_id bigint default null)
    returns setof fsm.state_machine as
    $$
      declare
        initial fsm.state%rowtype;
        machine fsm.state_machine%rowtype;
      begin

        if machine_id is not null
          then
              insert into fsm.state_machine (shard_id, id, statechart_id) values
                (shard, machine_id, statechart)
              returning * into machine;
          else
              insert into fsm.state_machine (shard_id, statechart_id) values
                (shard, statechart)
              returning * into machine;
        end if;

        for initial in select * from fsm.get_initial_state(statechart)
        loop

          insert into fsm.state_machine_state
            (shard_id, state_machine_id, statechart_id, state_id) values
            (shard, machine.id, machine.statechart_id, initial.id);

          if initial.on_entry is not null
          then
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

          return next machine;
        end loop;

        return;
      end
    $$ language plpgsql volatile;

COMMIT;

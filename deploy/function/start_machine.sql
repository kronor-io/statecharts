-- Deploy statecharts:function/start_machine to pg

BEGIN;

  create or replace function fsm.start_machine(statechart bigint, machine_id bigint default null)
    returns setof fsm.state_machine as
    $$
      declare
        initial fsm.state%rowtype;
        machine fsm.state_machine%rowtype;
      begin
        for initial in
            select *
            from fsm.state
            where statechart_id = statechart
              and is_initial
              and parent_id is null
        loop
          if machine_id is not null
          then
              insert into fsm.state_machine (id, statechart_id) values
                (machine_id, statechart)
              returning * into machine;
          else
              insert into fsm.state_machine (statechart_id) values
                (statechart)
              returning * into machine;
          end if;

          insert into fsm.state_machine_state
            (state_machine_id, statechart_id, state_id) values
            (machine.id, machine.statechart_id, initial.id);

          if initial.on_entry is not null
          then
              execute format('select %I.%I(%L::fsm_event_payload)',
                  coalesce((initial.on_entry).schema_name, 'public')
                , (initial.on_entry).function_name
                , ( machine.id
                  , '__initial__'
                  , '{}'::jsonb
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
    $$ language plpgsql;

COMMIT;

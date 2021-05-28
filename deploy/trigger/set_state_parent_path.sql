-- Deploy statecharts:trigger/set_state_parent_path to pg

BEGIN;

  create or replace function fsm.trig_set_state_parent_path() returns trigger as
  $$
      declare
          path ltree;
      begin

          if NEW.parent_id is null then
              NEW.parent_path = coalesce(NEW.statechart_id, OLD.statechart_id)::text::ltree;

          elseif TG_OP = 'INSERT' or OLD.parent_id is null or OLD.parent_id != NEW.parent_id then
              select parent_path || id
              from fsm.state
              where id = NEW.parent_id and statechart_id = NEW.statechart_id and not is_final
              into path;

              if path is null then
                  raise exception 'Invalid parent_id. It should exist and not be final: %', NEW.parent_id;
              end if;

              new.parent_path = path;
          end if;

          new.node_path = new.parent_path || new.id;

          return new;
      end;
  $$ language plpgsql;

  create trigger set_state_parent_path
      before insert or update on fsm.state
      for each row execute procedure fsm.trig_set_state_parent_path();

COMMIT;

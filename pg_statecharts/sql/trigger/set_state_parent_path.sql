-- Written without ltree operators or casts on purpose. This trigger fires while
-- pg_restore loads fsm.state, and pg_restore runs with an empty search_path,
-- where a bare "ltree" or "||" cannot be resolved. The paths are built as text
-- and assigned to the ltree columns, which PL/pgSQL converts through the type's
-- input function. ltree labels never contain a dot, so joining with '.' is
-- exactly what the || operator does.
create or replace function fsm.trig_set_state_parent_path() returns trigger as
$$
    declare
        parent_path_ text;
    begin

        if NEW.parent_id is null then
            parent_path_ := coalesce(NEW.statechart_id, OLD.statechart_id)::text;

        elseif TG_OP = 'INSERT' or OLD.parent_id is null or OLD.parent_id != NEW.parent_id then
            select parent_path::text || '.' || id
            from fsm.state
            where id = NEW.parent_id and statechart_id = NEW.statechart_id and not is_final
            into parent_path_;

            if parent_path_ is null then
                raise exception 'Invalid parent_id. It should exist and not be final: %', NEW.parent_id;
            end if;

        else
            parent_path_ := NEW.parent_path::text;
        end if;

        NEW.parent_path := parent_path_;
        NEW.node_path := parent_path_ || '.' || NEW.id;

        return NEW;
    end;
$$ language plpgsql;

create trigger set_state_parent_path
    before insert or update on fsm.state
    for each row execute procedure fsm.trig_set_state_parent_path();

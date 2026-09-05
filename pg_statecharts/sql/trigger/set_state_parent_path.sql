-- The ltree type and its || operator are written with an explicit schema. This
-- trigger fires while pg_restore loads fsm.state, and pg_restore runs with an
-- empty search_path, where a bare "ltree" or "||" cannot be resolved and the
-- restore of the fsm data fails. Everything pg_statecharts owns lives in fsm
-- and is already qualified; ltree is installed into public. (ltree is
-- relocatable, so a database that keeps it in another schema has to change
-- these references, but the rest of the extension assumes it is on the
-- search_path anyway, which in practice means public.)
create or replace function fsm.trig_set_state_parent_path() returns trigger as
$$
    declare
        path public.ltree;
    begin

        if NEW.parent_id is null then
            NEW.parent_path = coalesce(NEW.statechart_id, OLD.statechart_id)::text::public.ltree;

        elseif TG_OP = 'INSERT' or OLD.parent_id is null or OLD.parent_id != NEW.parent_id then
            select parent_path operator(public.||) id
            from fsm.state
            where id = NEW.parent_id and statechart_id = NEW.statechart_id and not is_final
            into path;

            if path is null then
                raise exception 'Invalid parent_id. It should exist and not be final: %', NEW.parent_id;
            end if;

            new.parent_path = path;
        end if;

        new.node_path = new.parent_path operator(public.||) new.id;

        return new;
    end;
$$ language plpgsql;

create trigger set_state_parent_path
    before insert or update on fsm.state
    for each row execute procedure fsm.trig_set_state_parent_path();

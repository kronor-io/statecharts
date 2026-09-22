-- Parsing .scxml documents into fsm.scxml_state / fsm.scxml_transition rows.
--
-- Everything here matches elements by local-name() rather than by namespace,
-- so a document is accepted whether or not it declares the SCXML namespace.
--
-- Attributes are always read through xmltable rather than by casting the
-- result of xpath() to text. Casting re-escapes the XML special characters, so
-- a state named "Pick &amp; Pack" would come back as "Pick &amp;amp; Pack";
-- xmltable hands back the decoded value.

create or replace function fsm.__xml_attr(node xml, attribute_name text)
returns text as
$$
  select value
  from xmltable('/*' passing node columns value text path ('@' || attribute_name))
$$ language sql immutable;

-- Collects the <script src="..."/> children of an <onentry>/<onexit> element
-- into the callback array shape that fsm.state stores.
--
-- A src without a dot is taken to live in the public schema, otherwise it is
-- split on the first dot: "billing.charge_card" is charge_card in the billing
-- schema.
create or replace function fsm.__scxml_callbacks(node xml, element_name text)
returns fsm_callback_name[] as
$$
  select coalesce(
    array_agg(
      case
        when strpos(src, '.') = 0
          then row('public', src)::fsm_callback_name
        else
          row(
            left(src, strpos(src, '.') - 1),
            substr(src, strpos(src, '.') + 1)
          )::fsm_callback_name
      end
      order by ord
    ),
    array[]::fsm_callback_name[]
  )
  from xmltable(
    ('/*/*[local-name()="' || element_name || '"]/*[local-name()="script"]')
    passing node
    columns
      src text path '@src',
      ord for ordinality
  )
$$ language sql immutable strict;

-- Walks one <state>/<final>/<parallel> element and everything below it,
-- emitting one row per state in document order.
--
-- parent_id and is_initial cannot be worked out from the element itself, so
-- the caller passes them down: the parent knows which of its children the
-- <initial> element pointed at.
create or replace function fsm.__scxml_states(
  node xml,
  parent_id text,
  is_initial boolean
)
returns setof fsm.scxml_state as
$$
  declare
    tag text;
    state_id text;
    state_name text;
    is_final boolean;
    initial_target text;
    children xml[];
    child xml;
  begin
    tag := (xpath('local-name(/*)', node))[1]::text;
    state_id := fsm.__xml_attr(node, 'id');

    if state_id is null then
      raise exception 'found a <%> element without an id attribute', tag
        using hint = 'every state, final and parallel element needs a unique id';
    end if;

    -- Matches the previous behaviour: an unnamed state gets an empty name
    -- rather than being rejected.
    state_name := coalesce(fsm.__xml_attr(node, 'name'), '');
    is_final := tag = 'final';

    return next row(
      state_id,
      state_name,
      parent_id,
      is_initial,
      is_final,
      fsm.__scxml_callbacks(node, 'onentry'),
      -- a final state is never exited, so it can have no on_exit callbacks
      case
        when is_final then array[]::fsm_callback_name[]
        else fsm.__scxml_callbacks(node, 'onexit')
      end
    )::fsm.scxml_state;

    children := xpath(
      '/*/*[local-name()="state" or local-name()="final" or local-name()="parallel"]',
      node
    );

    if coalesce(array_length(children, 1), 0) = 0 then
      return;
    end if;

    if tag = 'parallel' then
      -- Every region of a parallel state is entered at once, so all of its
      -- children are initial and an <initial> element would be meaningless.
      foreach child in array children loop
        return query select * from fsm.__scxml_states(child, state_id, true);
      end loop;

      return;
    end if;

    initial_target := (
      select fsm.__xml_attr(t, 'target')
      from unnest(
        xpath('/*/*[local-name()="initial"]/*[local-name()="transition"]', node)
      ) as t
      limit 1
    );

    if initial_target is null then
      raise exception 'state "%" has child states but no initial state', state_id
        using hint = format(
          'add <initial><transition target="..."/></initial> inside <%s id="%s">',
          tag, state_id
        );
    end if;

    if not exists (
      select 1 from unnest(children) as c where fsm.__xml_attr(c, 'id') = initial_target
    ) then
      raise exception 'state "%" declares "%" as its initial state, but has no such child',
        state_id, initial_target
        using detail = format(
          'child states are: %s',
          (
            select coalesce(string_agg(fsm.__xml_attr(c, 'id'), ', '), '(none)')
            from unnest(children) as c
          )
        );
    end if;

    foreach child in array children loop
      return query
        select *
        from fsm.__scxml_states(child, state_id, fsm.__xml_attr(child, 'id') = initial_target);
    end loop;
  end;
$$ language plpgsql immutable;

-- All the states in a document, in document order.
create or replace function fsm.scxml_states(doc xml)
returns setof fsm.scxml_state as
$$
  declare
    root_initial text;
    children xml[];
    child xml;
  begin
    root_initial := fsm.__xml_attr(doc, 'initial');

    if root_initial is null then
      raise exception 'the <scxml> element has no initial attribute'
        using hint = 'add initial="<id of the starting state>" to <scxml>';
    end if;

    children := xpath(
      '/*[local-name()="scxml"]/*[local-name()="state" or local-name()="final" or local-name()="parallel"]',
      doc
    );

    if coalesce(array_length(children, 1), 0) = 0 then
      raise exception 'the statechart has no states';
    end if;

    if not exists (
      select 1 from unnest(children) as c where fsm.__xml_attr(c, 'id') = root_initial
    ) then
      raise exception 'the statechart declares "%" as its initial state, but has no such top level state',
        root_initial
        using detail = format(
          'top level states are: %s',
          (
            select string_agg(fsm.__xml_attr(c, 'id'), ', ')
            from unnest(children) as c
          )
        );
    end if;

    foreach child in array children loop
      return query
        select *
        from fsm.__scxml_states(child, null, fsm.__xml_attr(child, 'id') = root_initial);
    end loop;
  end;
$$ language plpgsql immutable strict;

-- All the transitions in a document, in document order.
--
-- Transitions inside <initial> are excluded because they are how a compound
-- state names its default child, not real transitions.
--
-- Every other <transition> has to carry both an event and a target, and must
-- not sit inside a <final> state. Eventless and targetless transitions exist
-- in SCXML but this implementation has never supported them: fsm.transition
-- has NOT NULL on both columns, and a final state cannot be left. They are
-- rejected here, by name, rather than left to surface as a bare NOT NULL
-- violation from the importer or -- worse -- be dropped from a generated
-- migration because string_agg skips the NULL row.
create or replace function fsm.scxml_transitions(doc xml)
returns setof fsm.scxml_transition as
$$
  declare
    t record;
  begin
    for t in
      select
        tagged.tag,
        fsm.__xml_attr(states.node, 'id') as source_state,
        x.event,
        x.target
      from unnest(
        xpath('//*[local-name()="state" or local-name()="parallel" or local-name()="final"]', doc)
      ) with ordinality as states(node, state_ord)
      cross join lateral (
        select (xpath('local-name(/*)', states.node))[1]::text as tag
      ) as tagged(tag)
      cross join lateral xmltable(
        '/*/*[local-name()="transition"]'
        passing states.node
        columns
          event text path '@event',
          target text path '@target',
          transition_ord for ordinality
      ) as x
      order by states.state_ord, x.transition_ord
    loop
      if t.tag = 'final' then
        raise exception 'final state "%" has a transition, but a final state cannot be left',
          t.source_state
          using hint = 'remove the <transition> from <final id="' || t.source_state || '">, '
                       'or make it a <state>';
      end if;

      if t.event is null or t.event = '' then
        raise exception 'a transition out of state "%" has no event attribute', t.source_state
          using hint = 'every <transition> needs event="..." and target="..."; '
                       'eventless transitions are not supported';
      end if;

      if t.target is null or t.target = '' then
        raise exception 'the transition on event "%" out of state "%" has no target attribute',
          t.event, t.source_state
          using hint = 'every <transition> needs event="..." and target="..."; '
                       'targetless (internal) transitions are not supported';
      end if;

      return next row(t.event, t.source_state, t.target)::fsm.scxml_transition;
    end loop;
  end;
$$ language plpgsql immutable strict;

-- Reads and parses a .scxml file, reporting which file failed rather than just
-- that some XML somewhere was malformed.
create or replace function fsm.__read_scxml(file_path text)
returns xml as
$$
  declare
    contents text;
    doc xml;
    root_tag text;
  begin
    begin
      contents := pg_read_file(file_path);
    exception when others then
      raise exception 'failed to read %: %', file_path, sqlerrm
        using hint = 'the file is read by the PostgreSQL server, so it has to '
                     'exist on the database host and be readable by the '
                     'postgres process';
    end;

    begin
      -- xmlparse(document ...) rather than ::xml, because the default
      -- XMLOPTION is CONTENT, under which any old text is a valid XML
      -- fragment. Casting "hello" to xml succeeds; parsing it as a document
      -- fails, which is what we want.
      doc := xmlparse(document contents);
    exception when others then
      raise exception 'failed to parse SCXML in %: %', file_path, sqlerrm
        using detail = 'the file has to be a well formed XML document';
    end;

    root_tag := (xpath('local-name(/*)', doc))[1]::text;

    if root_tag is distinct from 'scxml' then
      raise exception '% is not an SCXML document', file_path
        using detail = format('the root element is <%s>, expected <scxml>', root_tag);
    end if;

    return doc;
  end;
$$ language plpgsql stable strict;

-- The chart name declared on the <scxml> element.
create or replace function fsm.__scxml_name(doc xml, file_path text)
returns text as
$$
  declare
    chart_name text;
  begin
    chart_name := fsm.__xml_attr(doc, 'name');

    if chart_name is null or chart_name = '' then
      raise exception 'the <scxml> element in % has no name attribute', file_path
        using hint = 'add name="<statechart name>" to <scxml>';
    end if;

    return chart_name;
  end;
$$ language plpgsql immutable;

-- The chart version declared on the <scxml> element, exactly as written.
--
-- The raw string is returned rather than an fsm.semver because it is what ends
-- up in the generated migration's file name and in its fsm.to_semver(...) call.
-- Validation still happens here so that a bad version is reported against the
-- file it came from.
create or replace function fsm.__scxml_version(doc xml, file_path text)
returns text as
$$
  declare
    raw_version text;
    normalised fsm.semver;
  begin
    raw_version := fsm.__xml_attr(doc, 'version');

    if raw_version is null or raw_version = '' then
      raise exception 'the <scxml> element in % has no version attribute', file_path
        using hint = 'add version="1.0.0" to <scxml>';
    end if;

    begin
      normalised := fsm.to_semver(raw_version);
    exception when others then
      raise exception 'the <scxml> element in % has an invalid version %',
        file_path, quote_literal(raw_version)
        using hint = 'the version has to look like 1, 1.2 or 1.2.3';
    end;

    return raw_version;
  end;
$$ language plpgsql immutable;

-- A pure SQL replacement for the `semver` PGXN extension.
--
-- The only reason `semver` was ever a dependency is that `fsm.statechart`
-- needs a version column that sorts correctly. `semver` is a C extension, so
-- depending on it means every user needs a compiler and the PostgreSQL server
-- headers just to install pg_statecharts. A domain over text gets us the same
-- storage and the same textual representation with no build step at all.
--
-- The one thing we lose is type level ordering: `order by version` on a text
-- domain is lexicographic, which would rank 1.9.0 above 1.10.0. Use
-- fsm.semver_sort_key() whenever versions need to be compared or ordered.

create domain fsm_semver as text
  check (value ~ '^\d+\.\d+\.\d+$');

comment on domain fsm_semver is $comment$
    A semantic version of the form <major>.<minor>.<patch>, e.g. '1.10.0'.

    Prerelease and build metadata suffixes (1.0.0-rc1, 1.0.0+build5) are not
    supported.

    Beware that this is a domain over text, so the default ordering is
    lexicographic and therefore wrong: '1.9.0' sorts after '1.10.0'. Order by
    fsm.semver_sort_key(version) instead.
$comment$;

create or replace function to_semver(version text)
returns fsm_semver as
$$
  declare
    dot_count integer;
    padded text;
  begin
    if version is null then
      return null;
    end if;

    dot_count := length(version) - length(replace(version, '.', ''));

    if dot_count > 2 then
      raise exception 'invalid version %', quote_literal(version)
        using hint = 'expected at most three components, e.g. 1.2.3';
    end if;

    -- pads with '.0' if the provided value has fewer than two dots, so that
    -- both '1' and '1.2' are accepted and normalised to '1.0.0' and '1.2.0'
    padded := version || repeat('.0', 2 - dot_count);

    if padded !~ '^\d+\.\d+\.\d+$' then
      raise exception 'invalid version %', quote_literal(version)
        using hint = 'expected digits separated by dots, e.g. 1.2.3';
    end if;

    return padded::fsm_semver;
  end;
$$ language plpgsql immutable;

comment on function to_semver(text) is $comment$
    Casts text to fsm_semver, padding out omitted components so that '1' and
    '1.2' become '1.0.0' and '1.2.0'.

    Kept in the public schema under its original name so that statechart
    migrations generated against older versions of pg_statecharts (which called
    the `semver` extension's to_semver) keep working unchanged.
$comment$;

create or replace function fsm.semver_sort_key(version fsm_semver)
returns integer[] as
$$
  select string_to_array(version, '.')::integer[]
$$ language sql immutable strict parallel safe;

comment on function fsm.semver_sort_key(fsm_semver) is $comment$
    Turns a version into an integer array so that it sorts numerically rather
    than lexicographically. Integer arrays compare element by element, so
    '1.10.0' correctly sorts above '1.9.0'.

        order by fsm.semver_sort_key(version) desc

    The function is immutable, so it can also be used to build an index:

        create index on fsm.statechart (name, fsm.semver_sort_key(version));
$comment$;

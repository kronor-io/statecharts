-- Finding, reading and writing files.
--
-- All of this happens on the database *server*, not on the client, so the
-- paths are paths on the database host and the postgres process needs
-- permission to use them. Running against Postgres in a container means the
-- paths have to be the ones inside the container.
--
-- The calling role needs pg_read_server_files to find and read .scxml files
-- and pg_write_server_files to generate migrations (superuser has both):
--
--     grant pg_read_server_files, pg_write_server_files to my_dev_user;

-- Strips trailing slashes so that joining paths never produces "a//b".
create or replace function fsm.__normalise_path(path text)
returns text as
$$
  select case
    when path is null then null
    when path = '' then ''
    else coalesce(nullif(regexp_replace(path, '/+$', ''), ''), '/')
  end
$$ language sql immutable;

-- Every .scxml file at or below source_path, in sorted order.
--
-- source_path may be a single .scxml file or a directory. Directories are only
-- descended into when recursive is true, and hidden entries are skipped.
create or replace function fsm.__find_scxml_files(source_path text, recursive boolean)
returns setof text as
$$
  declare
    path text := fsm.__normalise_path(source_path);
    entry text;
    child text;
  begin
    if path is null or path = '' then
      raise exception 'no source path given';
    end if;

    if pg_stat_file(path, true) is null then
      raise exception 'path does not exist: %', path
        using hint = 'the path is resolved on the database host, so when '
                     'Postgres runs in a container it has to be the path '
                     'inside the container';
    end if;

    if not (pg_stat_file(path)).isdir then
      if path !~ '\.scxml$' then
        raise exception 'not an .scxml file: %', path;
      end if;

      return next path;
      return;
    end if;

    for entry in select f from pg_ls_dir(path) as f loop
      child := path || '/' || entry;

      if (pg_stat_file(child)).isdir then
        if recursive and entry !~ '^\.' then
          return query select * from fsm.__find_scxml_files(child, true);
        end if;
      elsif entry ~ '\.scxml$' and entry !~ '^\.' then
        return next child;
      end if;
    end loop;
  end;
$$ language plpgsql stable;

-- Writes content to file_path, replacing whatever was there.
--
-- There is no "write a file" function in SQL, so this goes through COPY. Text
-- format would mangle backslashes and newlines, so the content is split into
-- one row per line and written as CSV with a delimiter and quote character
-- that cannot occur in the content. CSV never escapes backslashes, and with
-- nothing to quote the rows land in the file byte for byte. Empty lines are
-- written as NULL because CSV renders an empty *string* as a quoted "".
--
-- The one thing this cannot reproduce is a file that does not end in a
-- newline, because COPY terminates every row it writes. Content without a
-- trailing newline gets one. Every file this extension generates ends in a
-- newline anyway, so it makes no difference in practice.
create or replace function fsm.__write_file(file_path text, content text)
returns void as
$$
  declare
    dir text;
  begin
    -- deliberately not STRICT: a strict function returns NULL without running,
    -- which would turn "write this file" into a silent no-op
    if file_path is null then
      raise exception 'no output path given';
    end if;

    if content is null then
      raise exception 'refusing to write % because the content is null', file_path;
    end if;

    if content ~ E'[\r\x01\x02]' then
      raise exception 'refusing to write % because its content contains a carriage return or a control character',
        file_path
        using detail = 'those characters cannot survive the COPY based writer intact';
    end if;

    -- COPY quotes a line that is exactly "\." whatever the quote setting,
    -- because unquoted it would be read back as end of data. Nothing this
    -- extension generates contains such a line; refuse rather than corrupt.
    if content ~ E'(^|\n)\\\\\\.(\n|$)' then
      raise exception 'refusing to write % because a line consists of just a backslash and a dot',
        file_path
        using detail = 'COPY would quote that line and the file would no longer be written byte for byte';
    end if;

    if file_path !~ '/' then
      -- a bare file name is resolved against the data directory
      dir := '.';
    else
      dir := coalesce(nullif(regexp_replace(file_path, '/[^/]*$', ''), ''), '/');
    end if;

    if pg_stat_file(dir, true) is null then
      raise exception 'output directory does not exist: %', dir
        using hint = format('create it first, for example: mkdir -p %s', dir);
    end if;

    if not (pg_stat_file(dir)).isdir then
      raise exception 'not a directory: %', dir;
    end if;

    -- checked rather than "if not exists" so that repeated calls in one
    -- transaction do not each emit a NOTICE
    if to_regclass('pg_temp.__pg_statecharts_write') is null then
      create temp table __pg_statecharts_write (
        ord bigint,
        line text
      ) on commit drop;
    else
      truncate __pg_statecharts_write;
    end if;

    -- COPY terminates every row with a newline, so a trailing newline in the
    -- content would otherwise come out doubled.
    insert into __pg_statecharts_write (ord, line)
    select ord, nullif(line, '')
    from unnest(
      string_to_array(
        case when right(content, 1) = E'\n' then left(content, -1) else content end,
        E'\n'
      )
    ) with ordinality as lines(line, ord);

    begin
      execute format(
        'copy (select line from __pg_statecharts_write order by ord) to %L '
        'with (format csv, delimiter %L, quote %L, escape %L)',
        file_path, E'\x01', E'\x02', E'\x02'
      );
    exception when others then
      raise exception 'failed to write %: %', file_path, sqlerrm
        using hint = 'the file is written by the PostgreSQL server, so the '
                     'postgres process needs write permission on the '
                     'directory. When Postgres runs in a container, start the '
                     'container as your own user so the generated files end up '
                     'owned by you: docker compose run --user "$(id -u):$(id -g)"';
    end;
  end;
$$ language plpgsql volatile;

# Compares the pg_statecharts installed in one database with the one installed
# in another. Sourced, not run.
#
# Two checks need this. check_upgrade_matches_fresh.sh installs an older
# release and upgrades it; check_sqitch_adoption.sh adopts a sqitch deployment
# and upgrades that. Both are asking the same question -- does this route to
# the current version arrive where CREATE EXTENSION would have? -- and the
# answer is only worth anything if both ask it the same way, so the queries
# live here rather than in either script.
#
# Expects $PSQL to be set to a psql invocation with ON_ERROR_STOP.

# Every object the extension owns, described the same way in any database.
# This proves nothing was missed or left over; the rest describe what those
# objects actually are.
_cmp_members="
  select pg_describe_object(classid, objid, objsubid)
  from pg_depend
  where refclassid = 'pg_extension'::regclass
    and refobjid = (select oid from pg_extension where extname = 'pg_statecharts')
    and deptype = 'e'
  order by 1"

# Structure rather than names: a column left on an old type, a constraint that
# was never recreated, an index or trigger defined differently. pg_dump is no
# help here, because it deliberately omits extension members.
_cmp_definitions="
  with members as (
    select classid, objid
    from pg_depend
    where refclassid = 'pg_extension'::regclass
      and refobjid = (select oid from pg_extension where extname = 'pg_statecharts')
      and deptype = 'e'
  ),
  tables as (select objid from members where classid = 'pg_class'::regclass),
  types as (select objid from members where classid = 'pg_type'::regclass)
  select def from (
    select pg_describe_object('pg_class'::regclass, a.attrelid, a.attnum)
           || ' :: ' || format_type(a.atttypid, a.atttypmod)
           || case when a.attnotnull then ' not null' else '' end
           || coalesce(' default ' || pg_get_expr(d.adbin, d.adrelid), '') as def
    from pg_attribute a
    join tables t on t.objid = a.attrelid
    left join pg_attrdef d on d.adrelid = a.attrelid and d.adnum = a.attnum
    where a.attnum > 0 and not a.attisdropped

    union all

    select pg_describe_object('pg_constraint'::regclass, c.oid, 0)
           || ' :: ' || pg_get_constraintdef(c.oid)
    from pg_constraint c
    where c.conrelid in (select objid from tables)
       or c.contypid in (select objid from types)

    union all

    select pg_describe_object('pg_class'::regclass, i.indexrelid, 0)
           || ' :: ' || pg_get_indexdef(i.indexrelid)
    from pg_index i
    where i.indrelid in (select objid from tables)

    union all

    select pg_describe_object('pg_trigger'::regclass, tg.oid, 0)
           || ' :: ' || pg_get_triggerdef(tg.oid)
    from pg_trigger tg
    where tg.tgrelid in (select objid from tables)
      and not tg.tgisinternal
  ) d
  order by 1"

# The code itself. pg_get_functiondef carries the signature, the argument
# defaults, the volatility and strictness, the language and the body, so a
# function the upgrade forgot to replace shows up here whatever changed about
# it. This is the comparison nothing made before: the upgrade scripts hold
# their own copies of these bodies, written by hand, and a copy that falls
# behind its source leaves upgraded databases running last release's code with
# this release's version number on it.
_cmp_functions="
  select pg_get_functiondef(p.oid)
  from pg_proc p
  join pg_depend d
    on d.classid = 'pg_proc'::regclass and d.objid = p.oid and d.deptype = 'e'
  join pg_extension e on e.oid = d.refobjid and e.extname = 'pg_statecharts'
  order by p.oid::regprocedure::text"

# pg_dump only includes an extension's tables when they are registered, and
# with the right condition. Getting this wrong loses the machines out of every
# backup while keeping the application rows that point at them.
_cmp_dump_config="
  select c.oid::regclass::text || ' :: ' || coalesce(x.condition, '(no condition)')
  from pg_extension e
  cross join lateral unnest(e.extconfig, e.extcondition) as x(cfg, condition)
  join pg_class c on c.oid = x.cfg
  where e.extname = 'pg_statecharts'
  order by 1"

# The documentation a user reads out of \df+ and \d+.
_cmp_comments="
  select pg_describe_object(classoid, objoid, objsubid) || ' :: ' || description
  from pg_description
  where objoid in (
    select objid from pg_depend
    where refclassid = 'pg_extension'::regclass
      and refobjid = (select oid from pg_extension where extname = 'pg_statecharts')
      and deptype = 'e')
  order by 1"

# compare_extension FRESH_DB OTHER_DB DESCRIPTION [formatting]
#
# `formatting` is either "exact" or "ignoring-whitespace", and it applies to
# the two sections made of prose: function bodies and comments. Only the
# sqitch adoption asks for the second: adoption is ALTER EXTENSION ... ADD, so
# it takes ownership of what sqitch deployed rather than rewriting it, and
# sqitch indented it its own way. Every other route installs the text this
# repository ships, so anything less than exact there would be hiding
# something.
compare_extension() {
  local fresh=$1 other=$2 what=$3 formatting=${4:-exact}
  local section sql diffopts failed=0

  for section in members definitions functions dump_config comments; do
    eval "sql=\$_cmp_$section"
    diffopts=""
    case "$section:$formatting" in
      # -w ignores indentation inside a line, -B blank lines. A difference
      # that survives both is a difference in the code, or in what a comment
      # says. Comments get the same treatment as bodies and for the same
      # reason: sqitch wrote some of them, and it indented them its own way.
      functions:ignoring-whitespace|comments:ignoring-whitespace) diffopts="-w -B" ;;
    esac
    if ! diff $diffopts \
         <($PSQL -tA -d "$fresh" -c "$sql") \
         <($PSQL -tA -d "$other" -c "$sql"); then
      echo "error: $what does not match a fresh install: $section differ" >&2
      echo "(< fresh install, > $what)" >&2
      failed=1
    fi
  done

  if [ "$failed" -ne 0 ]; then
    return 1
  fi
  if [ "$formatting" = exact ]; then
    echo "$what matches a fresh install"
  else
    echo "$what matches a fresh install, ignoring whitespace in bodies and comments"
  fi
}

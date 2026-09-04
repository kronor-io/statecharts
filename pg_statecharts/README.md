# pg_statecharts

Statecharts (hierarchical state machines) in PostgreSQL, packaged as an
extension.

This is the half you install in production: the tables that hold statechart
definitions and running machines, and the functions that drive them. The
tooling that turns `.scxml` files into migrations lives in a separate
extension, [pg_statecharts_dev](../pg_statecharts_dev), so that nothing able to
read and write files on the database host has to be installed on a production
server.

## Installing

The extension is pure SQL. There is nothing to compile, so one set of files
works on every PostgreSQL version, architecture and operating system, and
installing is just copying two files into PostgreSQL's extension directory.

```bash
./install.sh
# or, for a PostgreSQL that is not first on your PATH:
./install.sh /usr/lib/postgresql/18/bin/pg_config
```

`install.sh` needs nothing but a shell and `pg_config`. If you would rather use
PGXS, `make install` does the same thing and produces the same files.

On Debian and Ubuntu there is a package per PostgreSQL major version on the
[releases page](https://github.com/kronor-io/statecharts/releases). It
contains only this extension, so it is the one to put on production servers:

```bash
sudo dpkg -i pg-statecharts-18_0.1.0.deb
```

The releases also carry a tarball with both extensions, whose top-level
`install.sh` installs just this one unless given `--dev`.

Then, in the database:

```sql
create extension pg_statecharts cascade;
```

`cascade` is there to pull in `ltree`, the only dependency. It ships with
PostgreSQL as part of the standard contrib modules, so there is nothing to
fetch or build.

## Versions

Tested against PostgreSQL 16, 17 and 18.

## Migrating to 0.1.0

There are three places a database can be starting from, and the ending point
is the same for all of them: the extension at 0.1.0, with the version column
on the built-in `fsm.semver` domain and the `semver` extension gone.

| Starting point | What to run |
|---|---|
| Nothing installed | `create extension pg_statecharts cascade;` |
| 0.0.0, the Rust build | `alter extension pg_statecharts update;` |
| Deployed from `deploy/` with sqitch | `create extension pg_statecharts version 'sqitch';` then `alter extension pg_statecharts update;` |

In every case the new extension files have to be installed first, as described
under [Installing](#installing). The two upgrades are covered by CI: one job
installs 0.0.0 with data and upgrades it, another deploys `deploy/` with
sqitch, adopts it, upgrades it and checks the result owns exactly the objects a
fresh install does.

### Fresh install

Nothing to migrate. `create extension pg_statecharts cascade` creates the
`fsm` schema and everything in it. Statecharts then come from `.scxml` files
through [pg_statecharts_dev](../pg_statecharts_dev), either imported directly
or as generated sqitch migrations.

### From 0.0.0, the Rust build

Install the new files as above and then:

```sql
alter extension pg_statecharts update;
```

On Debian the 0.0.0 package was called `pg-statecharts`, and the new
`pg-statecharts-<PG>` package declares that it replaces it, so a plain
`dpkg -i` removes the old package for you. That also removes the Rust shared
library, so the 0.0.0 functions stop working the moment the new package is
installed. Run the `alter extension` right after.

### From a sqitch deployment

Before there was an extension, this repository was deployed by running sqitch
against its `deploy/` directory. A database set up that way has every object
already, so it cannot `create extension` in the normal way. Instead, a version
named `sqitch` adopts the existing objects into the extension without creating
or changing anything, and the update then takes the same path as from 0.0.0,
because 0.0.0 was generated from `deploy/`:

```sql
create extension pg_statecharts version 'sqitch';
alter extension pg_statecharts update;
```

The `semver` extension has to still be installed on the server while this
runs, because converting the stored versions reads them through its output
function. Afterwards it can be uninstalled.

Do not put those two statements verbatim into a sqitch change. The `sqitch`
version only adopts, so on a database that has no legacy schema, which is what
every freshly created development database is, it fails with `schema "fsm"
does not exist` and rolls back. The same change has to run everywhere, so make
it decide:

```sql
do $migrate$
begin
  if exists (select 1 from pg_namespace where nspname = 'fsm')
     and not exists (select 1 from pg_extension where extname = 'pg_statecharts') then
    -- deployed from deploy/ with sqitch: adopt the existing objects
    create extension pg_statecharts version 'sqitch';
  else
    -- fresh database, or the extension is already installed
    create extension if not exists pg_statecharts cascade;
  end if;
  alter extension pg_statecharts update;
end
$migrate$;
```

This is one change that works from all three starting points. On a legacy
database it adopts and upgrades. On a fresh one it installs 0.1.0 and the
update is a no-op. On a 0.0.0 install it upgrades. Running it again does
nothing. CI runs this exact block against a sqitch deployment and against an
empty database, twice each, and checks both end up owning the same objects.

Two things change for the project that owns the database:

- **sqitch must not revert the fsm changes any more.** The extension protects
  the objects it owns, so most of those reverts fail. The dangerous ones are
  the reworks of `function/handle_machine_events`, whose revert scripts are
  `create or replace function` and go through, swapping an old body into a
  function the extension now owns. Leave the fsm changes in the plan as
  history.

- **Migrations generated by the Haskell SDK need a one-time rewrite.** The SDK
  writes versions as casts, `1.0::semver` or `'1.5.1'::semver`, in deploy,
  verify and revert scripts alike. That type no longer exists once the upgrade
  has dropped the `semver` extension, so fresh deploys, verifies and reverts of
  those changes fail. No shim can rescue a cast to a missing type. Replace the
  casts with `fsm.to_semver('1.0')` and `fsm.to_semver('1.5.1')` in the
  existing files; sqitch identifies a change by its plan entry, not by the
  script's contents. From the sqitch project directory:

  ```sh
  git grep -l '::semver' -- deploy revert verify | xargs sed -i -E \
    -e "s/'([0-9]+(\.[0-9]+){0,2})'::semver/fsm.to_semver('\1')/g" \
    -e "s/\b([0-9]+\.[0-9]+)::semver/fsm.to_semver('\1')/g"
  git grep -n '::semver' -- deploy revert verify   # should print nothing
  ```

  The quoted form goes first so the second expression cannot re-match inside
  the replacement. Migrations generated by 0.0.0 already use `to_semver(...)`,
  are not touched by this, and keep working through the compatibility alias.

  Do not regenerate the old migrations with `pg_statecharts_dev` to fix this.
  Its verify scripts also check that every callback function the chart
  references exists with the right signature, which is only true if those
  functions come before the statechart in the plan. Historical charts rarely
  satisfy that. Keep only the newest `.scxml` file for each chart under
  generator control, and fix the older migrations with the rewrite above.

### What the upgrade changes

The update script is the same for both upgrade paths. It converts
`fsm.statechart.version` off the `semver` extension's type onto the built-in
`fsm.semver` domain, removes the now unnecessary dependency on `semver`, and
drops the two file handling functions that 0.0.0 had and that have moved to
`pg_statecharts_dev`. Your data is preserved and migrations generated by 0.0.0
keep deploying, reverting and verifying unchanged.

Two things to know:

- **`semver` is dropped.** Once `fsm.statechart` no longer uses it, nothing in
  pg_statecharts does, and it has to go because it owns the name
  `to_semver(text)`. If anything else in your database still uses the `semver`
  type, the upgrade stops with an error naming exactly what depends on it and
  changes nothing.

- **The version column reads differently.** `fsm.semver` is a domain over
  `integer[]`, so a column that used to show `1.10.0` now shows `{1,10,0}`.
  The versions themselves are unchanged. Anything that displays a version, or
  compares one against a string, needs `fsm.semver_text()`:

  ```sql
  select name, fsm.semver_text(version) from fsm.statechart;
  ```

  Ordering and equality both need no helper. `order by version desc` is a
  numeric, component-by-component comparison, and
  `where version = fsm.to_semver('1.2.3')` works as before:

  ```sql
  -- correct with no sort key: 1.10.0 outranks 1.9.0 because 10 > 9
  select * from fsm.statechart order by version desc;
  ```

  Integers were chosen over text precisely so that this is right by default.
  A text version column sorts lexicographically and would rank `1.9.0` above
  `1.10.0` unless every caller remembered a helper.

  There is one way to lose that. `ORDER BY` resolves a bare name against the
  **output** columns first, so rendering to text and reusing the name `version`
  shadows the integer column and quietly restores lexicographic ordering:

  ```sql
  -- WRONG: orders by the rendered text, giving 2.0.0, 1.9.0, 1.10.0
  select fsm.semver_text(version) as version from fsm.statechart order by version desc;

  -- right: the alias does not shadow the column
  select fsm.semver_text(version) as rendered from fsm.statechart order by version desc;
  ```

Prerelease versions (`1.0.0-rc1`) are not supported. 0.0.0 accepted them,
because the `semver` extension does, so the upgrade checks for them first and
refuses with a list of the offending rows rather than failing part way through.
Rename those versions to plain `major.minor.patch` and run the update again.

After upgrading, `pg_statecharts.so` is no longer referenced by anything and
can be deleted from `$(pg_config --pkglibdir)`.

## Versions and the `semver` extension

Everything version related lives in the `fsm` schema: the `fsm.semver` domain,
`fsm.to_semver(text)` and `fsm.semver_text(fsm.semver)`. Use those names.

`fsm.semver` is a three element `integer[]` — `1.10.0` is `array[1, 10, 0]`.
Storing integers rather than text is what makes `order by version desc` correct
without a helper function, which is why the column no longer renders as
`1.10.0` on its own. Nothing here needs the `semver` extension.

One unqualified `to_semver(text)` is also created, because migrations generated
by 0.0.0 call it. It is only a thin alias for `fsm.to_semver(text)`, and it is
**skipped** when something already owns that name — most obviously the
[`semver`](https://pgxn.org/dist/semver/) extension, which is where that name
came from in the first place. So:

- A database with no `semver` extension gets the alias, and migrations
  generated by 0.0.0 keep working with no action from you.
- A database that uses `semver` for its own unrelated purposes can now install
  pg_statecharts at all, which it could not before. `create extension` reports
  the skip as a `WARNING` (PostgreSQL runs extension scripts with
  `client_min_messages` at `warning`, so a `NOTICE` would not be seen).
  Migrations generated by 0.0.0 will not deploy there until they are
  regenerated against 0.1.0.

Regenerating a chart with 0.1.0 rewrites bare `to_semver(...)` calls to
`fsm.to_semver(...)`, so expect that one-line diff per migration the first time
you regenerate.

## Layout

`sql/` holds one file per object. `sql/parts.txt` lists them in creation order,
and both `make` and `install.sh` read that list to assemble the single
`pg_statecharts--<version>.sql` script that PostgreSQL actually loads. Add a
new object by adding its file and putting it in the right place in
`parts.txt`.

`sql/upgrade/` holds handwritten `--<from>--<to>.sql` scripts.

## Tests

```bash
make install
make installcheck          # needs a server you can connect to
```

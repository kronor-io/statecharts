# Statecharts for PostgreSQL

A statechart interpreter built entirely inside PostgreSQL. Define hierarchical state machines as data, send events, and let the database execute transitions — including `on_entry`/`on_exit` callbacks, composite states, parallel regions, and automatic "done" events.

---

## Table of Contents

- [What are Statecharts?](#what-are-statecharts)
- [Why run Statecharts in the database?](#why-run-statecharts-in-the-database)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Database Schema Overview](#database-schema-overview)
- [Usage Guide](#usage-guide)
  - [1 — Define a statechart](#1--define-a-statechart)
  - [2 — Create and start a machine](#2--create-and-start-a-machine)
  - [3 — Send events and process transitions](#3--send-events-and-process-transitions)
  - [4 — Query the current state](#4--query-the-current-state)
  - [5 — Callbacks: on\_entry and on\_exit](#5--callbacks-on_entry-and-on_exit)
  - [6 — Composite (hierarchical) states](#6--composite-hierarchical-states)
  - [7 — Parallel states](#7--parallel-states)
  - [8 — Done events](#8--done-events)
- [Function Reference](#function-reference)
- [Haskell SDK — SCXML to SQL Code Generator](#haskell-sdk--scxml-to-sql-code-generator)
- [Running the Tests](#running-the-tests)

---

## What are Statecharts?

Statecharts are an extension of classical finite state machines that add **hierarchical (nested) states**, **parallel regions**, **entry/exit actions**, and **automatic completion events**. The formalism is defined by the [W3C SCXML specification](https://www.w3.org/TR/scxml/) and is the basis of tools like XState.

A simple search-result viewer can be modelled like this:

```mermaid
stateDiagram-v2
    [*] --> initial
    initial --> searching : search
    searching --> displaying : results
    displaying --> zoomed_in : zoom
    zoomed_in --> displaying : zoom_out
```

Events drive transitions; the machine is always in exactly one active state (or in multiple states when parallel regions are used).

---

## Why run Statecharts in the database?

1. **Single source of truth** — the state lives next to the data it describes; no synchronisation lag between your app and DB.
2. **No hidden trigger spaghetti** — business logic is expressed as plain rows (`fsm.state`, `fsm.transition`) and functions, not opaque triggers scattered across multiple tables.
3. **ACID guarantees** — state transitions are committed atomically together with any data changes in the same transaction.
4. **Language-agnostic** — any client that can speak SQL can drive the machine.
5. **Persistent audit trail** — `fsm.state_machine_state` keeps a timestamped log of every state the machine has ever been in.
6. **Versioned definitions** — statecharts carry a semver so you can evolve your logic without breaking running machines.

---

## Prerequisites

| Dependency | Notes |
|---|---|
| PostgreSQL ≥ 13 | Tested on 13+ |
| [`ltree`](https://www.postgresql.org/docs/current/ltree.html) extension | Ships with PostgreSQL |
| [`semver`](https://pgxn.org/dist/semver/) extension | Only for the sqitch path; the extension needs no compiler and no `semver` |
| [sqitch](https://sqitch.org) | Change-management tool used to deploy migrations |

---

## Installation

There are two ways to install, and they are alternatives — pick one. A
database already deployed with sqitch can be moved over to the extension; see
[Migrating to 0.1.0](pg_statecharts/README.md#migrating-to-010).

### As an extension (recommended)

`pg_statecharts` packages the same schema as a PostgreSQL extension. It is pure
SQL, so there is nothing to compile: the same files work on every PostgreSQL
version, architecture and operating system, and `ltree` is the only dependency.

```bash
cd pg_statecharts
./install.sh          # copies two files into PostgreSQL's extension directory
```

```sql
create extension pg_statecharts cascade;
```

There is a second, optional extension for development machines,
[`pg_statecharts_dev`](pg_statecharts_dev), which turns `.scxml` files into
statecharts or into sqitch migrations. It reads and writes files on the
database host, so it is kept separate and is not something to install in
production.

Each [release](https://github.com/kronor-io/statecharts/releases) ships the
same files prepackaged:

| Artifact | Contents |
|---|---|
| `pg_statecharts-<version>.tar.gz` | Both extensions. `./install.sh` installs the runtime; `./install.sh --dev` installs both. |
| `pg-statecharts-<PG>_<version>.deb` | The runtime extension for PostgreSQL major `<PG>`. This is the one for production. |
| `pg-statecharts-dev-<PG>_<version>.deb` | The dev tooling. Depends on the runtime package of the same version. |

```bash
sudo dpkg -i pg-statecharts-18_0.1.0.deb                             # production
sudo dpkg -i pg-statecharts-18_0.1.0.deb pg-statecharts-dev-18_0.1.0.deb  # development
```

See [pg_statecharts/README.md](pg_statecharts/README.md) for details, including
how to upgrade from the older Rust build and what that means for your
[backups](pg_statecharts/README.md#backups), and [example/](example) for a
complete working project.

### With sqitch

The original deployment path: clone the repository and run sqitch against your
database.

```bash
git clone https://github.com/kronor-io/statecharts
cd statecharts
sqitch deploy -t postgresql://user:password@host/db_name
```

This creates the `fsm` schema and installs all tables, types, triggers, and functions.

To roll back:

```bash
sqitch revert -t postgresql://user:password@host/db_name
```

Note that this path uses the [`semver`](https://pgxn.org/dist/semver/) PGXN
extension for the version column, whereas the extension defines an `fsm.semver`
domain over `integer[]` in plain SQL and needs no such dependency. Versions
therefore render as `{1,10,0}` rather than `1.10.0` under the extension; use
`fsm.semver_text(version)` to format one.

---

## Database Schema Overview

```mermaid
erDiagram
    statechart {
        bigint id PK
        text name
        integer[] version
        timestamptz created_at
    }
    state {
        bigint statechart_id FK
        text id PK
        text name
        text parent_id FK
        bool is_initial
        bool is_final
        fsm_callback_name[] on_entry
        fsm_callback_name[] on_exit
        ltree parent_path
        ltree node_path
    }
    transition {
        bigint statechart_id FK
        text event
        text source_state FK
        text target_state FK
    }
    state_machine {
        bigint shard_id
        bigint id PK
        bigint statechart_id FK
        timestamptz created_at
    }
    state_machine_state {
        bigint shard_id
        bigint state_machine_id FK
        text state_id FK
        timestamptz entered_at
        timestamptz exited_at
    }
    state_machine_event {
        bigint shard_id
        bigint id PK
        bigint state_machine_id FK
        text name
        jsonb data
        timestamptz created_at
        timestamptz handled_at
    }

    statechart ||--o{ state : "defines"
    statechart ||--o{ transition : "defines"
    state ||--o{ state : "parent_id (nested)"
    state_machine }o--|| statechart : "runs"
    state_machine ||--o{ state_machine_state : "tracks"
    state_machine ||--o{ state_machine_event : "queues"
```

**`fsm.statechart`** — versioned chart definitions.  
**`fsm.state`** — every node in the chart tree; nesting is expressed via `parent_id`.  
**`fsm.transition`** — which event moves the machine from `source_state` to `target_state`.  
**`fsm.state_machine`** — a running instance of a statechart.  
**`fsm.state_machine_state`** — append-only log; `exited_at IS NULL` marks the currently active state(s).  
**`fsm.state_machine_event`** — inbox queue; `handled_at IS NULL` means the event is pending.

---

## Usage Guide

### 1 — Define a statechart

Insert a statechart, its states, and its transitions. The example below is the search viewer from the diagram above:

```sql
-- 1. Register the statechart
-- fsm.to_semver pads '1.0' out to 1.0.0. Under the sqitch path, where the
-- version column is the semver extension's type, write '1.0'::semver instead.
INSERT INTO fsm.statechart (id, name, version)
VALUES (1, 'search_viewer', fsm.to_semver('1.0'));

-- 2. Define the states
INSERT INTO fsm.state
  (statechart_id, id,           name,                 parent_id, is_initial, is_final)
VALUES
  (1, 'initial',    'Initial',            NULL,      TRUE,  FALSE),
  (1, 'searching',  'Searching',          NULL,      FALSE, FALSE),
  (1, 'displaying', 'Displaying Results', NULL,      FALSE, FALSE),
  (1, 'zoomed_in',  'Zoomed In',          NULL,      FALSE, FALSE);

-- 3. Define the transitions
INSERT INTO fsm.transition
  (statechart_id, event,      source_state, target_state)
VALUES
  (1, 'search',   'initial',    'searching'),
  (1, 'results',  'searching',  'displaying'),
  (1, 'zoom',     'displaying', 'zoomed_in'),
  (1, 'zoom_out', 'zoomed_in',  'displaying');
```

> **Tip:** Use the [Haskell SDK](#haskell-sdk--scxml-to-sql-code-generator) to generate these SQL files from SCXML sources instead of writing them by hand.

---

### 2 — Create and start a machine

Arguments are named throughout the examples below. Positional calls work just
as well, but `shard => 1` reads better than a bare `1`. Note that the parameter
names are not yet consistent between functions — the shard is `shard` here,
`shard_id` on one function and `shid` on another — so copy them from the
[Function Reference](#function-reference) rather than guessing.

A *statechart* is a definition; a *state machine* is a running instance of that definition. You need both:

```sql
-- Create the instance (does not enter any state yet)
SELECT id AS machine_id
FROM fsm.create_machine(
    shard      => 1,   -- logical partition key; use your application's tenant/shard id
    statechart => 1
) \gset

-- Start it: enters the initial state and fires on_entry callbacks
SELECT fsm.start_machine(shard => 1, machine_id => :machine_id);
```

Or use the convenience function that does both in one call with the latest chart version:

```sql
SELECT fsm.start_machine_with_latest_statechart(
    shard_id => 1,
    named    => 'search_viewer'
);
```

---

### 3 — Send events and process transitions

**Queue an event:**

```sql
SELECT fsm.notify_state_machine(
    shard   => 1,
    machine => :machine_id,
    event   => 'search',
    data    => '{"query": "cats"}'   -- jsonb, defaults to '{}'
);
```

**Process all pending events:**

```sql
SELECT fsm.handle_machine_events(shard => 1, machine_id => :machine_id);
```

`handle_machine_events` picks up every unhandled event in insertion order, looks up the matching transition for each currently active state, exits the source state tree (firing `on_exit` callbacks), enters the target state tree (firing `on_entry` callbacks), and records the new active states — all in a single PL/pgSQL loop.

Multiple events can be queued before calling `handle_machine_events`; they will be processed sequentially:

```sql
SELECT fsm.notify_state_machine(shard => 1, machine => :machine_id, event => 'search',  data => '{"query": "cats"}');
SELECT fsm.notify_state_machine(shard => 1, machine => :machine_id, event => 'results', data => '{"count": 42}');
SELECT fsm.notify_state_machine(shard => 1, machine => :machine_id, event => 'zoom');

SELECT fsm.handle_machine_events(shard => 1, machine_id => :machine_id);
-- Machine is now in state 'zoomed_in'
```

---

### 4 — Query the current state

**Active state(s):**

```sql
SELECT state_id, entered_at
FROM fsm.state_machine_state
WHERE shard_id        = 1
  AND state_machine_id = :machine_id
  AND exited_at IS NULL;
```

**Check whether a specific state is active:**

```sql
-- note the abbreviated parameter names on this one
SELECT fsm.is_state_active(shid => 1, smid => :machine_id, state => 'zoomed_in');
```

**Check whether an event would trigger a transition:**

```sql
SELECT fsm.is_valid_transition(shard => 1, machine_id => :machine_id, event_ => 'zoom_out');
```

---

### 5 — Callbacks: on\_entry and on\_exit

Each state can call one or more PostgreSQL functions when it is entered or exited. Callbacks receive an `fsm_event_payload` argument:

```sql
CREATE TYPE fsm_event_payload AS (
    shard_id     bigint,
    machine_id   bigint,
    event_name   text,
    data         jsonb,   -- the payload passed to notify_state_machine
    from_state   text,
    to_state     text,
    payload_type text     -- 'on_entry' or 'on_exit'
);
```

**Example — audit log callback:**

```sql
CREATE OR REPLACE FUNCTION public.audit_state_change(p fsm_event_payload)
RETURNS void LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO audit_log (machine_id, event, from_state, to_state, data, ts)
    VALUES (p.machine_id, p.event_name, p.from_state, p.to_state, p.data, now());
END;
$$;
```

Register it on a state:

```sql
UPDATE fsm.state
SET on_entry = ARRAY[('public', 'audit_state_change')]::fsm_callback_name[]
WHERE statechart_id = 1
  AND id = 'displaying';
```

Multiple callbacks are stored as an array and invoked in array order.

---

### 6 — Composite (hierarchical) states

A state becomes a *composite* (compound) state when other states reference it via `parent_id`. One child must be marked `is_initial = TRUE`; it is entered automatically when the parent is entered.

```mermaid
stateDiagram-v2
    [*] --> initial
    initial --> research : search

    state research {
        [*] --> searching
        state searching {
            [*] --> googling
            [*] --> binging
            state googling {
                [*] --> checking_google
                checking_google --> got_google : google_results
                got_google : got_google [final]
            }
            state binging {
                [*] --> checking_bing
                checking_bing --> got_bing : bing_results
                got_bing : got_bing [final]
            }
        }
        searching --> read_results : done.state.searching
        read_results : read_results [final]
    }
```

SQL definition:

```sql
INSERT INTO fsm.state
  (statechart_id, id,               name,              parent_id,   is_initial, is_final)
VALUES
  (2, 'initial',        'Initial',         NULL,        TRUE,  FALSE),
  (2, 'research',       'Research',        NULL,        FALSE, FALSE),
  (2, 'searching',      'Searching',       'research',  TRUE,  FALSE),
  (2, 'googling',       'Googling',        'searching', TRUE,  FALSE),
  (2, 'checking_google','Do the Googling', 'googling',  TRUE,  FALSE),
  (2, 'got_google',     'Got Google',      'googling',  FALSE, TRUE),
  (2, 'binging',        'Binging',         'searching', TRUE,  FALSE),
  (2, 'checking_bing',  'Do the Binging',  'binging',   TRUE,  FALSE),
  (2, 'got_bing',       'Got Bing',        'binging',   FALSE, TRUE),
  (2, 'read_results',   'Read',            'research',  FALSE, TRUE);

INSERT INTO fsm.transition
  (statechart_id, event,                  source_state,     target_state)
VALUES
  (2, 'search',               'initial',        'research'),
  (2, 'google_results',       'checking_google','got_google'),
  (2, 'bing_results',         'checking_bing',  'got_bing'),
  (2, 'done.state.searching', 'searching',      'read_results');
```

Sending `search` activates `research`, which immediately activates `searching`, which in turn activates `googling` → `checking_google` **and** `binging` → `checking_bing` (both are initial children at their respective levels).

---

### 7 — Parallel states

When multiple children of a composite state all have `is_initial = TRUE` at the same level, they run in *parallel* — the machine is simultaneously active in all of them.

```mermaid
stateDiagram-v2
    [*] --> searching

    state searching {
        state google_region {
            [*] --> googling
            googling --> googling_done : google.done
            googling_done : googling_done [final]
        }
        --
        state bing_region {
            [*] --> binging
            binging --> binging_done : bing.done
            binging_done : binging_done [final]
        }
    }

    searching --> search_complete : done.state.searching
    search_complete : search_complete [final]
```

When both `googling_done` and `binging_done` are reached (i.e. both parallel regions are in a final state), the engine automatically fires `done.state.searching`, which transitions the parent `searching` state to `search_complete`.

---

### 8 — Done events

When a machine transitions into a **final state** (`is_final = TRUE`) the engine automatically queues the event `done.state.<state_id>`. If the parent compound state also becomes fully complete (all parallel regions finished), `done.state.<parent_id>` is queued as well, cascading up the tree.

This means you never need to manually fire completion events — just define the final states and add transitions that react to `done.state.*` in the parent level.

---

## Function Reference

Parameter names are exactly as declared, so they can be used as named
arguments. They are **not consistent between functions** — the shard is
variously `shard`, `shard_id`, `shard_id_` and `shid`, and the machine is
`machine`, `machine_id` or `smid`. Copy from here rather than guessing.

| Function | Description |
|---|---|
| `fsm.create_machine(shard, statechart)` | Creates a machine instance without starting it. Returns the new `fsm.state_machine` row. |
| `fsm.start_machine(shard, machine_id [, initial_data])` | Enters the initial state(s) and fires their `on_entry` callbacks. |
| `fsm.create_state_machine_with_latest_statechart(shard_id_, named)` | Creates a machine using the highest-versioned statechart with the given name. |
| `fsm.start_machine_with_latest_statechart(shard_id, named [, initial_data])` | Creates **and** starts a machine with the latest chart version. |
| `fsm.get_latest_statechart(named)` | Returns the `fsm.statechart` row with the highest version for that name. |
| `fsm.notify_state_machine(shard, machine, event [, data])` | Queues an event for the machine. `data` defaults to `'{}'`. |
| `fsm.handle_machine_events(shard, machine_id)` | Processes all pending events in order, executes transitions and callbacks. |
| `fsm.is_state_active(shid, smid, state)` | Returns `true` if the machine is currently in the given state. |
| `fsm.is_valid_transition(shard, machine_id, event_)` | Returns `true` if the event would trigger a transition from the current state. |
| `fsm.get_initial_state(statechart)` | Returns the top-level initial state(s) of a statechart. |
| `fsm.to_semver(version)` | Parses a version string into `fsm.semver`, padding `1` and `1.2` out to `1.0.0` and `1.2.0`. |
| `fsm.semver_text(version)` | Renders an `fsm.semver` as `1.10.0`. Casting to text instead gives the array form, `{1,10,0}`. |

All functions live in the `fsm` schema.

---

## Haskell SDK — SCXML to SQL Code Generator

The `sdk/` directory contains a Haskell library and CLI tool that converts [SCXML](https://www.w3.org/TR/scxml/) files into sqitch-compatible SQL migration files, so you can design state machines visually and commit them to version control as code.

The SDK targets the sqitch installation path only. The migrations it writes
cast versions to the `semver` extension's type, which the extension path does
not have, so they fail against a database running the extension. Projects on
the extension generate migrations with
[`pg_statecharts_dev`](pg_statecharts_dev) instead, and projects moving from
sqitch to the extension rewrite the casts in their existing migrations once;
see [Migrating to 0.1.0](pg_statecharts/README.md#migrating-to-010).

### Build

```bash
cd sdk
cabal build
```

### Generate SQL from SCXML

```bash
cabal run generate-chart -- \
  --project    my_project \
  --database-dir ../database \
  --files      path/to/my_machine.scxml
```

This produces:
- `database/deploy/statechart/<name>-<version>.sql`
- `database/verify/statechart/<name>-<version>.sql`
- `database/revert/statechart/<name>-<version>.sql`

And appends the new migration to `sqitch.plan`.

If [plantuml](https://plantuml.com) is available in your `PATH`, an SVG diagram is also rendered alongside each SCXML file.

### SCXML example

```xml
<scxml xmlns="http://www.w3.org/2005/07/scxml"
       name="search_viewer"
       version="1.0"
       initial="initial">

  <state id="initial">
    <transition event="search" target="searching"/>
  </state>

  <state id="searching">
    <transition event="results" target="displaying"/>
  </state>

  <state id="displaying">
    <transition event="zoom" target="zoomed_in"/>
  </state>

  <state id="zoomed_in">
    <transition event="zoom_out" target="displaying"/>
  </state>

</scxml>
```

---

## Running the Tests

The SQL tests use [pgTAP](https://pgtap.org) and are run via [pg_prove](https://pgtap.org/pg_prove.html):

```bash
pg_prove -d postgresql://user:password@host/db_name test/**/*.pg
```


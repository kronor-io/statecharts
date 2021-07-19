-- Deploy statecharts:statechart_tables to pg

BEGIN;

CREATE TABLE fsm.statechart (
  id bigserial PRIMARY KEY,
  created_at timestamptz NOT NULL DEFAULT now(),
  name text NOT NULL,
  version semver NOT NULL,

  CONSTRAINT name_must_be_within_bounds
    CHECK (char_length(name) >= 1 and char_length(name) <= 200)
);

CREATE UNIQUE INDEX idx_unique_name_version ON fsm.statechart(name, version);

comment on table fsm.statechart is 'All the different statecharts and their versions.';

CREATE TYPE fsm_callback_name AS (
  schema_name name,
  function_name name
);

comment on type fsm_callback_name is
    'Type for on_entry and on_exit callbacks. Should contain both the schema and the function name to invoke';

CREATE TABLE fsm.state (
  statechart_id bigint NOT NULL,
  is_initial bool NOT NULL,
  is_final bool NOT NULL,
  id text NOT NULL,
  name text NOT NULL,
  parent_path ltree NOT NULL,
  node_path ltree NOT NULL,
  parent_id text,
  on_entry fsm_callback_name,
  on_exit fsm_callback_name,
  PRIMARY KEY (statechart_id, id),

  CONSTRAINT id_must_be_within_bounds
    CHECK (char_length(id) >= 1 and char_length(id) <= 50),

  CONSTRAINT id_must_be_alphanumeric
    CHECK (id ~ '^[a-zA-Z0-9_]+$'),

  CONSTRAINT cannot_be_both_initial_and_final
    CHECK (not (is_initial and is_final)),

  CONSTRAINT fk_statechart
    FOREIGN KEY(statechart_id)
    REFERENCES fsm.statechart(id)
    ON DELETE CASCADE,

  CONSTRAINT fk_parent_state
    FOREIGN KEY(statechart_id, parent_id)
    REFERENCES fsm.state(statechart_id, id)
    ON DELETE CASCADE
);

CREATE INDEX idx_parent ON fsm.state(statechart_id, parent_id);
CREATE INDEX idx_parent_path ON fsm.state USING GIST (parent_path) INCLUDE (statechart_id);
CREATE INDEX idx_node_path ON fsm.state USING GIST (node_path) INCLUDE (statechart_id);

-- https://statecharts.dev/glossary/state.html
comment on table fsm.state is $comment$
    A state in a state machine describes a particular behaviour of the machine.
    When we say that a machine is “in” a state, it means that the machine behaves
    in the way that state describes.

    The behaviour of a state is defined as how the state reacts to events.
    Each state can specify a number of events that it “understands”, and specifies,
    for each event, any number of transitions that could be taken, if that event happens.
    A state also describes a set of actions to execute when the state is entered or
    exited, and in some implementations, activities that should be happening for the
    entire duration of time that the machine is in the state.

    When a state machine is executed, it enters the initial state, and adopts the
    behaviour associated with that state.  As the machine handles events, it
    transitions to other states, thus changing the behaviour of the machine over time.
$comment$;

-- https://statecharts.dev/glossary/initial-state.html
comment on column fsm.state.is_initial is $comment$
    The initial state in the state machine. There can only be one initial state
    in a state machine, but there can be many for parallel states.
    
    When a state machine starts, it starts by entering the machine’s initial state.
    Likewise, when a compound state is entered, its initial state is also entered.
    The initial state is not a separate state, but more like an indication of which
    state that the machine should start in by default.

    An initial state should be thought of as the default starting point of a compound state
    if a transition points directly to the compound state.
$comment$;

-- https://statecharts.dev/glossary/final-state.html
comment on column fsm.state.is_final is $comment$
    The final state in the state machine. There cannot be any transitions from
    the final state.

    A final state is a state in a compound state that designates that the compound
    state in question has completed, i.e. will not process any further events.
    Reaching a final state will generate an internal event, which in turn can allow
    other parts of the state machine to react to the fact that the compound state has
    “completed”.
$comment$;

comment on column fsm.state.parent_path is $comment$
    Parent path of the state. This is used to represent that a state is a
    child state, or part of a parallel state.

    This column is automatically set based on the "parent_id" column.
$comment$;

comment on column fsm.state.node_path is $comment$
    Path of the state that is represented from the root of the tree.

    This column is automatically set based on the "parent_id" column.
$comment$;

comment on column fsm.state.parent_id is $comment$
    Id of the parent state.
    NULL means that the state has no parent, i.e. it is not part of a composite
    nor parallel state.
$comment$;

comment on column fsm.state.on_entry is $comment$
    The function to execute when this state is entered.
$comment$;

comment on column fsm.state.on_exit is $comment$
    The function to execute when the state is exited.
$comment$;

CREATE TABLE fsm.transition (
  statechart_id bigint NOT NULL,
  event text NOT NULL,
  source_state text NOT NULL,
  target_state text NOT NULL,
  PRIMARY KEY (statechart_id, event, source_state),

  CONSTRAINT event_must_be_within_bounds
    CHECK (char_length(event) >= 1 and char_length(event) <= 100),

  CONSTRAINT fk_source_state
    FOREIGN KEY(statechart_id, source_state)
    REFERENCES fsm.state(statechart_id, id)
    ON DELETE CASCADE,

  CONSTRAINT fk_target_state
    FOREIGN KEY(statechart_id, target_state)
    REFERENCES fsm.state(statechart_id, id)
    ON DELETE CASCADE
);

-- https://statecharts.dev/glossary/transition.html
comment on table fsm.transition is $comment$
    In an executing state machine, a transition is the instantaneous transfer
    from one state to another.  In a state machine, a transition tells us
    what happens when an event occurs.
    
    When an event happens, the currently active state(s) are inspected,
    looking for an outbound transition that could be triggered by the event.
$comment$;

comment on column fsm.transition.event is 'Name of the transition event.';

comment on column fsm.transition.source_state is 'The from state of the transition.';

comment on column fsm.transition.target_state is 'The to state of the transition.';

COMMIT;

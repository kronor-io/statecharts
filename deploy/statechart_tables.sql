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

CREATE TABLE fsm.state (
  statechart_id bigint NOT NULL,
  is_initial bool NOT NULL,
  is_final bool NOT NULL,
  id text NOT NULL,
  name text NOT NULL,
  parent_path ltree NOT NULL,
  node_path ltree NOT NULL,
  parent_id text,
  on_entry text,
  on_exit text,
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
CREATE UNIQUE INDEX final_state_should_be_unique ON fsm.state(statechart_id, parent_path) WHERE is_final;
CREATE INDEX idx_parent_path ON fsm.state USING GIST (parent_path) INCLUDE (statechart_id);
CREATE INDEX idx_node_path ON fsm.state USING GIST (node_path) INCLUDE (statechart_id);

CREATE TYPE transition_scope AS ENUM (
  'internal',
  'external'
);

CREATE TABLE fsm.transition (
  statechart_id bigint NOT NULL,
  event text NOT NULL,
  source_state text NOT NULL,
  target_state text NOT NULL,
  scope transition_scope NOT NULL DEFAULT 'external',
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

COMMIT;

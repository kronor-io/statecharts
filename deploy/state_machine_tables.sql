-- Deploy statecharts:state_machine_tables to pg

BEGIN;

  CREATE TABLE fsm.state_machine (
    id bigserial NOT NULL PRIMARY KEY,
    statechart_id bigint NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now(),

    CONSTRAINT fk_statechart
      FOREIGN KEY(statechart_id)
      REFERENCES fsm.statechart(id)
      ON DELETE CASCADE
  );

  CREATE INDEX idx_statechart ON fsm.state_machine(statechart_id);

  CREATE TABLE fsm.state_machine_state (
    state_machine_id bigint NOT NULL,
    statechart_id bigint NOT NULL,
    entered_at timestamptz NOT NULL DEFAULT now(),
    exited_at timestamptz,
    state_id text NOT NULL,

    CONSTRAINT entered_should_be_before_exited
      CHECK (entered_at <= exited_at),

    CONSTRAINT fk_state_machine
      FOREIGN KEY(state_machine_id)
      REFERENCES fsm.state_machine(id)
      ON DELETE CASCADE,

    CONSTRAINT fk_state
      FOREIGN KEY(statechart_id, state_id)
      REFERENCES fsm.state(statechart_id, id)
      ON DELETE CASCADE
  );

  CREATE UNIQUE INDEX idx_state ON fsm.state_machine_state(state_machine_id, state_id)
    WHERE exited_at IS NULL;

  CREATE TABLE fsm.state_machine_event (
    id bigserial NOT NULL PRIMARY KEY,
    state_machine_id bigint NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now(),
    handled_at timestamptz,
    name text NOT NULL,
    data jsonb NOT NULL,

    CONSTRAINT created_should_be_before_handled
      CHECK (created_at <= handled_at),

    CONSTRAINT fk_state_machine
      FOREIGN KEY(state_machine_id)
      REFERENCES fsm.state_machine(id)
      ON DELETE CASCADE
  );

  CREATE INDEX idx_state_machine ON fsm.state_machine_event(state_machine_id)
    WHERE handled_at IS NULL;

COMMIT;

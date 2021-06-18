-- Deploy statecharts:state_machine_tables to pg

BEGIN;

  CREATE TABLE fsm.state_machine (
    shard_id bigint NOT NULL,
    id bigserial NOT NULL,
    statechart_id bigint NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now(),

    PRIMARY KEY (shard_id, id),

    CONSTRAINT fk_statechart
      FOREIGN KEY(statechart_id)
      REFERENCES fsm.statechart(id)
      ON DELETE CASCADE
  );

  CREATE INDEX idx_statechart ON fsm.state_machine(statechart_id);

  CREATE TABLE fsm.state_machine_state (
    shard_id bigint NOT NULL,
    state_machine_id bigint NOT NULL,
    statechart_id bigint NOT NULL,
    entered_at timestamptz NOT NULL DEFAULT now(),
    exited_at timestamptz,
    state_id text NOT NULL,

    CONSTRAINT entered_should_be_before_exited
      CHECK (entered_at <= exited_at),

    CONSTRAINT fk_state_machine
      FOREIGN KEY(shard_id, state_machine_id)
      REFERENCES fsm.state_machine(shard_id, id)
      ON DELETE CASCADE,

    CONSTRAINT fk_state
      FOREIGN KEY(statechart_id, state_id)
      REFERENCES fsm.state(statechart_id, id)
      ON DELETE CASCADE
  );

  CREATE UNIQUE INDEX idx_state ON fsm.state_machine_state(shard_id, state_machine_id, state_id)
    WHERE exited_at IS NULL;

  CREATE TABLE fsm.state_machine_event (
    shard_id bigint NOT NULL,
    id bigserial NOT NULL,
    state_machine_id bigint NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now(),
    handled_at timestamptz,
    name text NOT NULL,
    data jsonb NOT NULL,

    PRIMARY KEY (shard_id, id),

    CONSTRAINT created_should_be_before_handled
      CHECK (created_at <= handled_at),

    CONSTRAINT fk_state_machine
      FOREIGN KEY(shard_id, state_machine_id)
      REFERENCES fsm.state_machine(shard_id, id)
      ON DELETE CASCADE
  );

  CREATE INDEX idx_state_machine ON fsm.state_machine_event(shard_id, state_machine_id)
    WHERE handled_at IS NULL;

COMMIT;

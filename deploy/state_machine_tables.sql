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

  comment on table fsm.state_machine is
      'Runtime information of a statechart. Contains state machine instances that have started.';

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

  comment on table fsm.state_machine_state is $comment$
    Contains a log of the states that have been activated by a given state machine and
    is used to determine which states are currently active.
  $comment$;

  comment on column fsm.state_machine_state.entered_at is $comment$
      The timestamp when the state machine has transitioned to this state.
      This is the column that indicates that a state is currently active
      for a state machine.
      Should be before exited_at.
  $comment$;

  comment on column fsm.state_machine_state.exited_at is $comment$
      The timestamp when the state machine has transitioned away from this state.
      Should be after entered_at.
  $comment$;

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
  
  comment on table fsm.state_machine_event is
      'Queue of events that need to be processed for the state machine.';

  comment on column fsm.state_machine_event.created_at $comment$
      The timestamp when this event was created. The timestamp should be
      before "handled_at".
  $comment$;

  comment on column fsm.state_machine_event.handled_at $comment$
      The timestamp when this event was handled. The timestamp should be
      after "created_at".
  $comment$;

   comment on column fsm.state_machine_event.data is $comment$
       The data to be sent to the callback handlers in on_entry, on_exit.
   $comment$;
COMMIT;

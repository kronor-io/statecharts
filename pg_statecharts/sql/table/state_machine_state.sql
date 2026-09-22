create table fsm.state_machine_state (
  shard_id bigint not null,
  state_machine_id bigint not null,
  statechart_id bigint not null,
  entered_at timestamptz not null default now(),
  exited_at timestamptz,
  state_id text not null,

  constraint entered_should_be_before_exited
    check (entered_at <= exited_at),

  constraint fk_state_machine
    foreign key(shard_id, state_machine_id)
    references fsm.state_machine(shard_id, id)
    on delete cascade,

  constraint fk_state
    foreign key(statechart_id, state_id)
    references fsm.state(statechart_id, id)
    on delete cascade
);

create unique index idx_state on fsm.state_machine_state(shard_id, state_machine_id, state_id)
  where exited_at is null;

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

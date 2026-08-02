create table fsm.state (
  statechart_id bigint not null,
  is_initial bool not null,
  is_final bool not null,
  id text not null,
  name text not null,
  parent_path ltree not null,
  node_path ltree not null,
  parent_id text,
  on_entry fsm_callback_name[] not null default array[]::fsm_callback_name[],
  on_exit fsm_callback_name[] not null default array[]::fsm_callback_name[],
  primary key (statechart_id, id),

  constraint id_must_be_within_bounds
    check (char_length(id) >= 1 and char_length(id) <= 50),

  constraint id_must_be_alphanumeric
    check (id ~ '^[a-za-z0-9_]+$'),

  constraint cannot_be_both_initial_and_final
    check (not (is_initial and is_final)),

  constraint fk_statechart
    foreign key(statechart_id)
    references fsm.statechart(id)
    on delete cascade,

  constraint fk_parent_state
    foreign key(statechart_id, parent_id)
    references fsm.state(statechart_id, id)
    on delete cascade
);

create index idx_parent on fsm.state(statechart_id, parent_id);
create index idx_parent_path on fsm.state using gist (parent_path) include (statechart_id);
create index idx_node_path on fsm.state using gist (node_path) include (statechart_id);

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
    The functions to execute when this state is entered.
$comment$;

comment on column fsm.state.on_exit is $comment$
    The functions to execute when the state is exited.
$comment$;

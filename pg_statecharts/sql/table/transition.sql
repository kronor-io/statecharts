create table fsm.transition (
  statechart_id bigint not null,
  event text not null,
  source_state text not null,
  target_state text not null,
  primary key (statechart_id, event, source_state),

  constraint event_must_be_within_bounds
    check (char_length(event) >= 1 and char_length(event) <= 100),

  constraint fk_source_state
    foreign key(statechart_id, source_state)
    references fsm.state(statechart_id, id)
    on delete cascade,

  constraint fk_target_state
    foreign key(statechart_id, target_state)
    references fsm.state(statechart_id, id)
    on delete cascade
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

-- Deploy kronor:statechart/chart3 to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('chart3', 0.1::semver) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
(chart, 's1', 's1n', null, true, false, Array[('table1', 'action5')], Array[]),
(chart, 's2', 's2n', null, false, false, Array[('table1', 'action4')], Array[]),
(chart, 's3', 's3n', null, false, false, Array[('table1', 'action3')], Array[]),
(chart, 's6', 's6n', null, false, true, Array[('table1', 'action1')], Array[]),
(chart, 's7', 's7n', null, false, true, Array[], Array[]),
(chart, 's8', 's8n', null, false, true, Array[], Array[]),
(chart, 's9', 's9n', null, false, true, Array[], Array[]),
(chart, 's4', 's4n', 's3', true, false, Array[], Array[]),
(chart, 's5', 's5n', 's3', false, false, Array[('table1', 'action2')], Array[]);
insert into fsm.transition (statechart_id, event, source_state, target_state) values
(chart, 'ev1', 's1', 's2'),
(chart, 'ev2', 's2', 's3'),
(chart, 'ev3', 's2', 's9'),
(chart, 'ev4', 's2', 's7'),
(chart, 'ev5', 's3', 's6'),
(chart, 'ev6', 's3', 's8'),
(chart, 'ev7', 's3', 's9'),
(chart, 'ev8', 's3', 's7'),
(chart, 'ev9', 's4', 's5'),
(chart, 'ev10', 's5', 's4');
end
$$;
COMMIT;

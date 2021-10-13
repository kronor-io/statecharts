-- Deploy kronor:statechart/chart2 to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('chart2', 0.1::semver) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
(chart, 's1', 's1n', null, true, false, null, null),
(chart, 's11', 's11n', null, false, true, null, null),
(chart, 's12', 's12n', null, false, true, null, null),
(chart, 's13', 's13n', null, false, true, ('table1', 'action11'), null),
(chart, 's2', 's2n', 's1', true, false, null, null),
(chart, 's8', 's8n', 's1', false, false, null, null),
(chart, 's3', 's3n', 's2', true, false, ('table1', 'action1'), null),
(chart, 's4', 's4n', 's2', false, false, ('table1', 'action2'), null),
(chart, 's5', 's5n', 's2', false, false, ('table1', 'action3'), null),
(chart, 's6', 's6n', 's2', false, false, ('table1', 'action4'), null),
(chart, 's7', 's7n', 's2', false, true, ('table1', 'action5'), null),
(chart, 's9', 's9n', 's8', true, false, ('table1', 'action7'), null),
(chart, 's10', 's10n', 's8', false, true, ('table1', 'action6'), null);
insert into fsm.transition (statechart_id, event, source_state, target_state) values
(chart, 'bar.abc3', 's1', 's11'),
(chart, 'bar.abc2', 's1', 's12'),
(chart, 'foo.abc1', 's1', 's13'),
(chart, 'foo.abc6', 's2', 's8'),
(chart, 'foo.ev10', 's3', 's4'),
(chart, 'foo.ev9', 's4', 's5'),
(chart, 'foo.ev8', 's5', 's6'),
(chart, 'foo.abc7', 's6', 's7'),
(chart, 'foo.abc5', 's9', 's10'),
(chart, 'foo.abc4', 's9', 's10');
end
$$;
COMMIT;

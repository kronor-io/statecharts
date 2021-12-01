-- Deploy kronor:statechart/chart1 to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('chart1', 0.1::semver) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
(chart, 's1', 's1n', null, true, false, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's5', 's5n', null, false, false, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's10', 's10n', null, false, false, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's16', 's16n', null, false, true, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's2', 's2n', 's1', true, false, array[]::fsm_callback_name[], array[('table1', 'action3'),('table1', 'action4')]::fsm_callback_name[]),
(chart, 's3', 's3n', 's1', false, false, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's4', 's4n', 's1', false, true, array[('table1', 'action1'),('table1', 'action2')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's6', 's6n', 's5', true, false, array[('table2', 'action')]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's7', 's7n', 's5', false, false, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's8', 's8n', 's5', false, false, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's17', 's17n', 's5', false, true, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's9', 's9n', 's5', false, true, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's11', 's11n', 's10', true, false, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's12', 's12n', 's10', false, false, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's13', 's13n', 's10', false, false, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's14', 's14n', 's10', false, true, array[]::fsm_callback_name[], array[]::fsm_callback_name[]),
(chart, 's15', 's15n', 's10', false, true, array[]::fsm_callback_name[], array[]::fsm_callback_name[]);
insert into fsm.transition (statechart_id, event, source_state, target_state) values
(chart, 'ev1', 's1', 's16'),
(chart, 'ev2', 's1', 's5'),
(chart, 'ev3', 's2', 's3'),
(chart, 'ev4', 's3', 's4'),
(chart, 'ev5', 's5', 's10'),
(chart, 'ev6', 's6', 's8'),
(chart, 'ev7', 's6', 's9'),
(chart, 'ev8', 's6', 's17'),
(chart, 'ev8', 's8', 's9'),
(chart, 'ev9', 's8', 's7'),
(chart, 'ev10', 's11', 's14'),
(chart, 'ev11', 's11', 's12'),
(chart, 'ev12', 's12', 's14'),
(chart, 'ev13', 's12', 's15'),
(chart, 'ev14', 's12', 's13'),
(chart, 'ev15', 's13', 's14'),
(chart, 'ev16', 's13', 's15'),
(chart, 'ev17', 's13', 's14'),
(chart, 'ev18', 's13', 's15');
end
$$;
COMMIT;

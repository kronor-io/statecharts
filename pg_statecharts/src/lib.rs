use pgrx::*;

mod gen_charts;
mod semver;


#[pg_schema]
mod fsm {
    use pgrx::*;

    /*
     * PGRX randomly reorders these import statements unless you explicitly tell the
     * extension_sql_file macro that it requires something else to be run first. So everything must
     * be painstakingly listed as coming after the table definitions...
     */

    // types
    extension_sql_file!("../sql/type/fsm_event_payload.sql");
    
    // tables
    extension_sql_file!("../sql/table/statechart.sql", requires = ["fsm_event_payload"]);
    extension_sql_file!("../sql/table/state.sql", requires = ["state"]);
    extension_sql_file!("../sql/table/transition.sql", requires = ["state"]);
    extension_sql_file!("../sql/table/state_machine.sql", requires = ["transition"]);
    extension_sql_file!("../sql/table/state_machine_state.sql", requires = ["state_machine"]);
    extension_sql_file!("../sql/table/state_machine_event.sql", requires = ["state_machine_state"]);
    
    // functions
    extension_sql_file!("../sql/function/create_machine.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/create_state_machine_with_latest_statechart.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/get_finalized_parents.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/get_initial_state.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/get_latest_statechart.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/handle_machine_events.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/is_state_active.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/is_valid_transition.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/notify_state_machine.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/start_machine.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/function/start_machine_with_latest_statechart.sql", requires = ["state_machine_event"]);

    // triggers
    extension_sql_file!("../sql/trigger/check_no_cross_boundary_transitions.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/trigger/check_no_duplicate_event_handler.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/trigger/check_no_state_loops.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/trigger/check_no_transition_for_final.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/trigger/check_valid_initial_state.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/trigger/notify_machine_event.sql", requires = ["state_machine_event"]);
    extension_sql_file!("../sql/trigger/set_state_parent_path.sql", requires = ["state_machine_event"]);
}

pgrx::pg_module_magic!();

/*
extension_sql_file!(
    "../sql/pg_statecharts--0.1.0--tables.sql",
    name = "tables",
    requires = ["semver_type"] // bootstrap
);
*/


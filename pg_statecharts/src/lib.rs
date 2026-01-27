use pgrx::*;

// mod semver;
mod gen_charts;


#[pg_schema]
mod fsm {
    use crate::gen_charts;

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
    extension_sql_file!("../sql/table/state.sql", requires = ["statechart"]);
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

    // functions to load .scxml files into the database
    #[pg_extern(requires = ["state_machine_event"])]
    pub fn deploy_scxml_files(
        source_path: &str,
        recursive: default!(bool, false),
    ) -> Result<(), Box<dyn std::error::Error>> {
        gen_charts::deploy_scxml_files(source_path, recursive)
    }

    #[pg_extern(requires = ["state_machine_event"])]
    pub fn gen_statechart_sqitch_migrations(
        source_path: &str,
        sqitch_plan_file_path: &str,
        recursive: default!(bool, false),
    ) -> Result<(), Box<dyn std::error::Error>> {
        gen_charts::gen_statechart_sqitch_migrations(source_path, sqitch_plan_file_path, recursive)
    }
}

pgrx::pg_module_magic!();


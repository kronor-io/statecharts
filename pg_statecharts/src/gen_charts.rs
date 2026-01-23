use pgrx::*;

#[pg_schema]
mod fsm {
    use pgrx::*;
    use quick_xml::de::from_str;
    use serde::{Deserialize};
    use std::ffi::OsStr;
    use std::fs;
    use std::path::{Path, PathBuf};
    use walkdir::WalkDir;

    #[pg_extern]
    fn deploy_scxml_files(
        source_path: &str,
        recursive: default!(bool, false),
    ) -> Result<(), Box<dyn std::error::Error>> {
        let scxml_file_paths = find_scxml_file_paths(source_path, recursive);

        let migrations = scxml_file_paths
            .iter()
            .map(|file_path| {
                let scxml = read_scxml_file(file_path);

                generate_sql_migration(&scxml).deploy
            })
            .collect::<Vec<String>>();

        let migration = migrations.join("\n\n");

        match Spi::run(&migration) {
            Err(err) => pgrx::error!("Failed to execute migration: {}", err),
            _ => (),
        };

        /*
        pgrx::info!(
            "Successfully deployed statechart: {} v{}",
            scxml.name,
            scxml.version
        );
        */

        Ok(())
    }

    #[pg_extern]
    fn gen_statechart_sqitch_migrations(
        source_path: &str,
        sqitch_dir: &str,
        recursive: default!(bool, false),
    ) -> Result<(), Box<dyn std::error::Error>> {
        let scxml_file_paths = find_scxml_file_paths(source_path, recursive);

        let _ = scxml_file_paths
            .iter()
            .map(|file_path| {
                let scxml = read_scxml_file(file_path);
                let migration = generate_sql_migration(&scxml);

                (file_path, scxml, migration)
            })
            // collect to break laziness, we want to make sure everything parses before we create
            // migrations
            .collect::<Vec<(&PathBuf, SCXML, Migration)>>()
            .iter()
            .map(|(_file_path, scxml, migration)| {
                let migration_path = format!(
                    "statechart/{}-{}.sql",
                    scxml.name.replace(".", "/"),
                    scxml.version
                );

                let deploy_path = Path::new(sqitch_dir).join("deploy").join(&migration_path);
                let revert_path = Path::new(sqitch_dir).join("revert").join(&migration_path);
                let verify_path = Path::new(sqitch_dir).join("verify").join(&migration_path);

                fs::create_dir_all(&deploy_path.parent().unwrap()).unwrap();
                fs::create_dir_all(&revert_path.parent().unwrap()).unwrap();
                fs::create_dir_all(&verify_path.parent().unwrap()).unwrap();

                fs::write(&deploy_path, &migration.deploy).unwrap();
                fs::write(&revert_path, &migration.revert).unwrap();
                fs::write(&verify_path, &migration.verify).unwrap();

                pgrx::info!("deploy migration path: {}", &deploy_path.display());
            })
            .collect::<Vec<()>>();

        Ok(())
    }

    fn read_scxml_file(file_path: &PathBuf) -> SCXML {
        let xml_content = match fs::read_to_string(file_path) {
            Ok(xml_content) => xml_content,
            Err(err) => {
                pgrx::error!("Failed to read file '{}': {}", file_path.display(), err)
            }
        };

        match from_str(&xml_content) {
            Ok(scxml) => scxml,
            Err(err) => pgrx::error!(
                "Failed to parse SCXML from '{}': {}",
                file_path.display(),
                err
            ),
        }
    }

    fn find_scxml_file_paths(source_path: &str, recursive: bool) -> Vec<PathBuf> {
        let path = Path::new(source_path);

        if !path.exists() {
            pgrx::error!("Path does not exist: {}", source_path);
        }

        let mut scxml_file_paths = if path.is_file() {
            if path.extension() != Some(OsStr::new("scxml")) {
                pgrx::error!("File is not an .scxml file: {}", source_path);
            }

            vec![path.to_path_buf()]
        } else if recursive {
            WalkDir::new(source_path)
                .into_iter()
                .filter_entry(|entry| {
                    let not_hidden = !entry
                        .file_name()
                        .to_str()
                        .map(|s| s.starts_with("."))
                        .unwrap_or(false);

                    let is_scxml = entry.path().extension() == Some(OsStr::new("scxml"));

                    let is_dir = entry.file_type().is_dir();

                    not_hidden && (is_scxml || is_dir)
                })
                .filter_map(|e| e.ok()) // skip files/directories that we don't have access to
                .filter(|entry| entry.path().is_file())
                .map(|entry| entry.path().to_path_buf())
                .collect()
        } else {
            match fs::read_dir(path) {
                Err(err) => pgrx::error!("Failed reading directory: {}, error: {}", source_path, err),
                Ok(dir_content) => dir_content
                    .filter_map(|entry| entry.ok().map(|e| e.path()))
                    .filter(|path| !path.is_dir() && path.extension() == Some(OsStr::new("scxml")))
                    .collect(),
            }
        };

        scxml_file_paths.sort();

        return scxml_file_paths;
    }

    struct Migration {
        deploy: String,
        revert: String,
        verify: String,
    }

    fn generate_sql_migration(scxml: &SCXML) -> Migration {
        let deploy = {
            let states_and_transitions = {
                let states = scxml
                    .states
                    .to_states_and_transitions(None, scxml.initial.clone());
                let final_states = scxml
                    .final_states
                    .to_states_and_transitions(None, scxml.initial.clone());

                states.join(final_states)
            };

            // state rows
            let state_rows = states_and_transitions
                .states
                .iter()
                .map(|s| {
                    let parent_id = s.parent_id.as_ref().map(|pid| format!("'{}'", pid)).unwrap_or("null".to_string());
                    let on_entry = s.on_entry.iter().map(|(schema, name)| format!("('{}', '{}')", schema, name)).collect::<Vec<String>>().join(",");
                    let on_exit = s.on_exit.iter().map(|(schema, name)| format!("('{}', '{}')", schema, name)).collect::<Vec<String>>().join(",");

                    format!("(chart, '{}', '{}', {}, {}, {}, array[{}]::fsm_callback_name[], array[{}]::fsm_callback_name[])", s.id, s.name, parent_id, s.is_initial, s.is_final, on_entry, on_exit)
                })
                .collect::<Vec<String>>()
                .join(",\n");

            // transition rows
            let transition_rows = states_and_transitions
                .transitions
                .iter()
                .map(|t| {
                    format!(
                        "(chart, '{}', '{}', '{}')",
                        t.event, t.source_state, t.target_state
                    )
                })
                .collect::<Vec<String>>()
                .join(",\n");

            format!(
                r#"
        do $$
        declare
        chart bigint;
        begin
        insert into fsm.statechart (name, version) values ('{}', '{}'::semver) returning id into chart;
        insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
        {};
        insert into fsm.transition (statechart_id, event, source_state, target_state) values
        {};
        end
        $$;
        "#,
                scxml.name, scxml.version, state_rows, transition_rows
            )
        };

        let revert = format!(
            r#"
with chart as (
    delete from fsm.statechart
    where name = '{}'
    and version = {}::semver
    returning id
)
delete from fsm.state
    where statechart_id = (select id from chart);
"#,
            scxml.name, scxml.version
        );

        let verify = "".to_string();

        return Migration {
            deploy,
            revert,
            verify,
        };
    }

    // SQL SCXML representation
    #[derive(Debug, Deserialize, PartialEq)]
    struct SqlStatesAndTransitions {
        states: Vec<SqlScxmlState>,
        transitions: Vec<SqlScxmlTransition>,
    }

    impl SqlStatesAndTransitions {
        pub fn new() -> SqlStatesAndTransitions {
            SqlStatesAndTransitions {
                states: Vec::new(),
                transitions: Vec::new(),
            }
        }

        pub fn join(mut self, mut other: SqlStatesAndTransitions) -> Self {
            self.states.append(&mut other.states);
            self.transitions.append(&mut other.transitions);
            self
        }
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct SqlScxmlState {
        id: String,
        name: String,
        parent_id: Option<String>,
        is_initial: bool,
        is_final: bool,
        on_entry: Vec<(String, String)>,
        on_exit: Vec<(String, String)>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct SqlScxmlTransition {
        event: String,
        source_state: String,
        target_state: String,
    }

    trait ToStatesAndTransitions {
        fn to_states_and_transitions(
            &self,
            parent_id: Option<String>,
            parent_initial_id: String,
        ) -> SqlStatesAndTransitions;
    }

    impl ToStatesAndTransitions for State {
        fn to_states_and_transitions(
            &self,
            parent_id: Option<String>,
            parent_initial_id: String,
        ) -> SqlStatesAndTransitions {
            let this_state = SqlScxmlState {
                id: self.id.clone(),
                name: self.name.clone(),
                parent_id: parent_id,
                is_initial: self.id == parent_initial_id,
                is_final: false,
                on_entry: self
                    .on_entry
                    .iter()
                    .flat_map(|oe| &oe.scripts)
                    .map(|s| match s.src.find('.') {
                        None => ("public".to_string(), s.src.clone()),
                        Some(dot_pos) => {
                            let (schema, name_with_dot) = s.src.split_at(dot_pos);
                            let name = &name_with_dot[1..];
                            (schema.to_string(), name.to_string())
                        }
                    })
                    .collect(),
                on_exit: self
                    .on_exit
                    .iter()
                    .flat_map(|oe| &oe.scripts)
                    .map(|s| match s.src.find('.') {
                        None => ("public".to_string(), s.src.clone()),
                        Some(dot_pos) => {
                            let (schema, name_with_dot) = s.src.split_at(dot_pos);
                            let name = &name_with_dot[1..];
                            (schema.to_string(), name.to_string())
                        }
                    })
                    .collect(),
            };

            let transitions = self
                .transitions
                .to_states_and_transitions(Some(self.id.clone()), parent_initial_id.clone());

            let child_states = {
                let maybe_children_initial_id = self
                    .initial
                    .as_ref()
                    .map(|i| i.transition.as_ref())
                    .flatten()
                    .map(|t| t.target.clone());

                // If there are child states then we also need to have an initial state for this child
                // states
                match (
                    maybe_children_initial_id,
                    self.child_states.is_empty() && self.child_final_states.is_empty(),
                ) {
                    (Some(children_initial_id), _) => {
                        let child_states = self.child_states.to_states_and_transitions(
                            Some(self.id.clone()),
                            children_initial_id.clone(),
                        );

                        let child_final_states = self.child_final_states.to_states_and_transitions(
                            Some(self.id.clone()),
                            children_initial_id.clone(),
                        );

                        child_states.join(child_final_states)
                    }
                    (None, true) => SqlStatesAndTransitions::new(),
                    (None, false) => {
                        pgrx::error!("state {} has child states but not initial", self.id)
                    }
                }
            };

            let mut result = SqlStatesAndTransitions::new();
            result.states = vec![this_state];

            return result.join(transitions).join(child_states);
        }
    }

    impl ToStatesAndTransitions for FinalState {
        fn to_states_and_transitions(
            &self,
            parent_id: Option<String>,
            parent_initial_id: String,
        ) -> SqlStatesAndTransitions {
            let this_state = SqlScxmlState {
                id: self.id.clone(),
                name: self.name.clone(),
                parent_id: parent_id,
                is_initial: self.id == parent_initial_id,
                is_final: true,
                on_entry: self
                    .on_entry
                    .iter()
                    .flat_map(|oe| &oe.scripts)
                    .map(|s| match s.src.find('.') {
                        None => ("public".to_string(), s.src.clone()),
                        Some(dot_pos) => {
                            let (schema, name_with_dot) = s.src.split_at(dot_pos);
                            let name = &name_with_dot[1..];
                            (schema.to_string(), name.to_string())
                        }
                    })
                    .collect(),
                on_exit: vec![],
            };

            let child_states = {
                let maybe_children_initial_id = self
                    .initial
                    .as_ref()
                    .map(|i| i.transition.as_ref())
                    .flatten()
                    .map(|t| t.target.clone());

                // If there are child states then we also need to have an initial state for this child
                // states
                match (
                    maybe_children_initial_id,
                    self.child_states.is_empty() && self.child_final_states.is_empty(),
                ) {
                    (Some(children_initial_id), _) => {
                        let child_states = self.child_states.to_states_and_transitions(
                            Some(self.id.clone()),
                            children_initial_id.clone(),
                        );

                        let child_final_states = self.child_final_states.to_states_and_transitions(
                            Some(self.id.clone()),
                            children_initial_id.clone(),
                        );

                        child_states.join(child_final_states)
                    }
                    (None, true) => SqlStatesAndTransitions::new(),
                    (None, false) => {
                        pgrx::error!("state {} has child states but not initial", self.id)
                    }
                }
            };

            let mut result = SqlStatesAndTransitions::new();
            result.states = vec![this_state];

            return result.join(child_states);
        }
    }

    impl ToStatesAndTransitions for Transition {
        fn to_states_and_transitions(
            &self,
            parent_id: Option<String>,
            _parent_initial_id: String,
        ) -> SqlStatesAndTransitions {
            let transition = match parent_id {
                None => pgrx::error!("Can't have transition without parent_id"),
                Some(some_parent_id) => SqlScxmlTransition {
                    event: self.event.clone(),
                    source_state: some_parent_id.clone(),
                    target_state: self.target.clone(),
                },
            };

            let mut result = SqlStatesAndTransitions::new();
            result.transitions = vec![transition];

            return result;
        }
    }

    impl<T> ToStatesAndTransitions for Vec<T>
    where
        T: ToStatesAndTransitions,
    {
        fn to_states_and_transitions(
            &self,
            parent_id: Option<String>,
            parent_initial_id: String,
        ) -> SqlStatesAndTransitions {
            let mut result = SqlStatesAndTransitions::new();

            for item in self.iter() {
                let item_result =
                    item.to_states_and_transitions(parent_id.clone(), parent_initial_id.clone());
                result = result.join(item_result);
            }

            result
        }
    }

    // Raw SCXML representation (as represented in the XML)
    #[derive(Debug, Deserialize, PartialEq)]
    pub struct SCXML {
        #[serde(rename = "@xmlns")]
        pub xmlns: String,

        #[serde(rename = "@name")]
        pub name: String,

        #[serde(rename = "@version")]
        pub version: String,

        #[serde(rename = "@initial")]
        pub initial: String,

        #[serde(rename = "state", default)]
        pub states: Vec<State>,

        #[serde(rename = "final", default)]
        pub final_states: Vec<FinalState>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    pub struct State {
        #[serde(rename = "@id")]
        pub id: String,

        #[serde(rename = "@name")]
        pub name: String,

        #[serde(rename = "transition", default)]
        pub transitions: Vec<Transition>,

        #[serde(rename = "onentry", default)]
        pub on_entry: Vec<OnEntry>,

        #[serde(rename = "onexit", default)]
        pub on_exit: Vec<OnExit>,

        #[serde(rename = "state", default)]
        pub child_states: Vec<State>,

        #[serde(rename = "final", default)]
        pub child_final_states: Vec<FinalState>,

        #[serde(rename = "parallel", default)]
        pub child_parallel_states: Vec<State>,

        #[serde(rename = "initial", default)]
        pub initial: Option<Initial>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    pub struct Initial {
        #[serde(rename = "transition", default)]
        pub transition: Option<InitialTransition>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    pub struct InitialTransition {
        #[serde(rename = "@target")]
        pub target: String,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    pub struct FinalState {
        #[serde(rename = "@id")]
        pub id: String,

        #[serde(rename = "@name")]
        pub name: String,

        #[serde(rename = "onentry", default)]
        pub on_entry: Vec<OnEntry>,

        #[serde(rename = "state", default)]
        pub child_states: Vec<State>,

        #[serde(rename = "final", default)]
        pub child_final_states: Vec<FinalState>,

        #[serde(rename = "parallel", default)]
        pub child_parallel_states: Vec<State>,

        #[serde(rename = "initial", default)]
        pub initial: Option<Initial>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    pub struct Transition {
        #[serde(rename = "@event")]
        pub event: String,

        #[serde(rename = "@target")]
        pub target: String,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    pub struct OnEntry {
        #[serde(rename = "script", default)]
        pub scripts: Vec<Script>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    pub struct OnExit {
        #[serde(rename = "script", default)]
        pub scripts: Vec<Script>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    pub struct Script {
        #[serde(rename = "@src")]
        pub src: String,
    }
}

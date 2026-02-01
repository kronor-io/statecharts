use pgrx::*;
use quick_xml::de::from_str;
use serde::{Deserialize};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use regex::Regex;
use chrono::{Utc};
use std::io::Write;
use std::os::unix::fs::{PermissionsExt};
use std::io::Read;

pub fn import_scxml_files(
    source_path: &str,
    recursive: bool,
    on_conflict_do_nothing: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let scxml_file_paths = find_scxml_file_paths(source_path, recursive).map_err(pgrx_err)?;

    let placeholder_project = "placeholder_project".to_string();

    let migrations = scxml_file_paths
        .iter()
        .map(read_scxml_file)
        .collect::<Result<Vec<SCXML>, String>>()
        .map_err(pgrx_err)?
        .iter()
        .filter(|scxml| {
            if !on_conflict_do_nothing { return true; }

            /*
             * Yes, this is vulnerable to SQL injection and yes, I would have preffered to do one
             * query to find all the existing statecharts and compare them in memory instead of
             * doing one query per .scxml file.
             *
             * But the semver extension has some quirks that prevents this. `1.0::semver` becomes
             * `1.0.0` so if we get all the statechart rows with name and version then the version
             * in the database won't be the same as the version in the file if the file version
             * doesn't specify all three digits.
             *
             * Also providing arguments to Spi turns them into strings I think, but '1.0'::semver
             * doesn't work. If it's a string then all three values are required 🤷
             */
            let query = format!("select not exists (select 1 from fsm.statechart where name = $1 and version = {}::semver)", &scxml.version);
            let statechart_name = &scxml.name;
            match Spi::get_one_with_args::<bool>(&query, &[statechart_name.into()]) {
                Ok(Some(not_exists)) => {
                    if !not_exists { pgrx::info!("Skipping statechart {} v{} because it's already deployed", &scxml.name, &scxml.version); }
                    not_exists
                },
                Ok(None) => pgrx::error!("Impossible, received no rows from 'exists' query"),
                Err(err) => pgrx::error!("Error looking for existing statecharts: {}", err)
            }
        })
        .map(|scxml| {
            // removing transaction control with replace is a bit of a hack
            let without_transaction =
                generate_sql_migration(&scxml, &placeholder_project)?
                    .deploy
                    .replace("BEGIN;", "")
                    .replace("COMMIT;", "");

            Ok(without_transaction)
        })
        .collect::<Result<Vec<String>, String>>()
        .map_err(pgrx_err)?;

    let migration = migrations.join("\n\n");

    match Spi::run(&migration) {
        Err(err) => Err(pgrx_err(format!("Failed to deploy statecharts: {}", err))),
        _ => Ok(()),
    }
}

pub fn gen_statechart_sqitch_migrations(
    source_path: &str,
    sqitch_plan_file_path: &str,
    recursive: bool,
    file_permission_666: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut sqitch_plan_file =
        std::fs::OpenOptions::new()
            .read(true)
            .append(true)
            .open(sqitch_plan_file_path)
            .map_err(|err| pgrx_err(format!("Failed to read sqitch plan file '{}': {}", sqitch_plan_file_path, err)))?;

    let sqitch_plan_string = {
        let mut buffer = String::new();
        sqitch_plan_file.read_to_string(&mut buffer).unwrap();
        buffer
    };

    let sqitch_project =
        sqitch_plan_string
            .lines()
            .find(|line| line.starts_with("%project="))
            .map_or_else(
                || Err(pgrx_err("Couldn't find %project property in sqitch plan file".to_string())),
                |line| Ok(line.trim_start_matches("%project=").trim().to_string())
            )?;

    let sqitch_dir = Path::new(sqitch_plan_file_path).parent().unwrap();

    find_scxml_file_paths(source_path, recursive)
        .map_err(pgrx_err)?
        .iter()
        .map(|file_path| {
            let scxml = read_scxml_file(file_path)?;
            let migration = generate_sql_migration(&scxml, &sqitch_project)?;

            Ok((scxml, migration))
        })
        // collect to break laziness, we want to make sure everything parses before we create
        // migrations
        .collect::<Result<Vec<(SCXML, Migration)>, String>>()
        .map_err(pgrx_err)?
        .iter()
        .map(|(scxml, migration)| {
            let migration_name = scxml.migration_name();

            // first we check if the migration is already present in sqitch.plan
            let new_migration = {
                let migration_regex = {
                    let escaped_name = regex::escape(&migration_name);
                    let pattern = format!(r"^{}\b", escaped_name);
                    Regex::new(&pattern).unwrap()
                };

                if !sqitch_plan_string.lines().any(|line| migration_regex.is_match(line)) {
                    let timestamp_str = Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string();
                    let migration_line = format!(
                        "{} {} pg_statecharts <pg_statecharts@postgres> # {}\n",
                        &migration_name, timestamp_str, &migration_name
                    );

                    std::io::Write::write_all(&mut sqitch_plan_file, migration_line.as_bytes()).unwrap();

                    true
                } else {
                    false
                }
            };

            // now we generate the contents of the migration
            let migration_path = format!(
                "{}.sql",
                &migration_name
            );

            let deploy_path = sqitch_dir.join("deploy").join(&migration_path);
            let revert_path = sqitch_dir.join("revert").join(&migration_path);
            let verify_path = sqitch_dir.join("verify").join(&migration_path);

            create_sqitch_file(&deploy_path, &migration.deploy, file_permission_666)?;
            create_sqitch_file(&revert_path, &migration.revert, file_permission_666)?;
            create_sqitch_file(&verify_path, &migration.verify, file_permission_666)?;

            if new_migration {
                pgrx::info!("created new migration: {}", &migration_name);
            } else {
                pgrx::info!("updated existing migration: {}", &migration_name);
            }

            Ok(())
        })
        .collect::<Result<Vec<()>, String>>()
        .map_err(pgrx_err)?;

    Ok(())
}

fn pgrx_err(msg: String) -> Box<dyn std::error::Error> {
    Box::new(std::io::Error::new(std::io::ErrorKind::Other, msg))
}

/// If file_permission_666 is set then the file and the its parent directories (that didn't already
/// exist) will have their permissions set to be readable and writeable by all users on the
/// machine.
///
/// This is because if the database is running in a container then any newly created files will be
/// owned by the docker user and by default they won't be accessible by whatever user you are.
fn create_sqitch_file(path: &Path, content: &str, file_permission_666: bool) -> Result<(), String> {
    if !file_permission_666 {
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, content).unwrap();

        return Ok(());
    }

    // Create parent directories as needed and give them 777 permissions when they're created
    {
        // Track which directories we need to create
        let mut dirs_to_create = Vec::new();
        let mut current = path.parent().unwrap().to_path_buf();
        
        // Walk up to find which directories don't exist
        while !current.exists() && current.parent().is_some() {
            dirs_to_create.push(current.clone());
            current = current.parent().unwrap().to_path_buf();
        }
        
        // Create directories from top to bottom
        for dir in dirs_to_create.iter().rev() {
            fs::create_dir(dir).unwrap();
            let perms = fs::Permissions::from_mode(0o777);
            fs::set_permissions(dir, perms).map_err(|err| format!("Error while setting dir permissions: {}", err))?;
        }
    }

    let mut file = std::fs::OpenOptions::new()
        .write(true)        // Open file for writing
        .create(true)       // Create if it doesn't exist
        .truncate(true)     // Clear existing contents if not empty
        .open(path)
        .map_err(|err| format!("Error while creating file: {}, {}", &path.display(), err))?;
        
    file.write_all(content.as_bytes()).unwrap();

    // Set permissions to rw-rw-rw-
    std::fs::set_permissions(
        path,
        std::fs::Permissions::from_mode(0o666)
    ).unwrap();

    Ok(())
}

fn read_scxml_file(file_path: &PathBuf) -> Result<SCXML, String> {
    let xml_content =
        fs::read_to_string(file_path)
            .map_err(|err| format!("Failed to read file '{}': {}", file_path.display(), err))?;

    from_str(&xml_content)
        .map_err(|err| format!("Failed to parse SCXML in '{}': {}", file_path.display(), err))
}

fn find_scxml_file_paths(source_path: &str, recursive: bool) -> Result<Vec<PathBuf>, String> {
    let path = Path::new(source_path);

    if !path.exists() {
        return Err(format!("Path does not exist: {}", source_path));
    }

    let mut scxml_file_paths = if path.is_file() {
        if path.extension() != Some(OsStr::new("scxml")) {
            return Err(format!("File is not an .scxml file: {}", source_path));
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
            Err(err) => return Err(format!("Failed reading directory: {}, error: {}", source_path, err)),
            Ok(dir_content) => dir_content
                .filter_map(|entry| entry.ok().map(|e| e.path()))
                .filter(|path| !path.is_dir() && path.extension() == Some(OsStr::new("scxml")))
                .collect(),
        }
    };

    scxml_file_paths.sort();

    return Ok(scxml_file_paths);
}

struct Migration {
    deploy: String,
    revert: String,
    verify: String,
}

fn generate_sql_migration(scxml: &SCXML, project: &String) -> Result<Migration, String> {
    let deploy = {
        let states_and_transitions =
            scxml.states.to_states_and_transitions(
                None,
                &|sid: &String| scxml.initial == *sid
            )?;

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
r#"-- Deploy {}:{} to pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('{}', {}::semver) returning id into chart;
insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
{};
insert into fsm.transition (statechart_id, event, source_state, target_state) values
{};
end
$$;
COMMIT;
"#,
            project, scxml.migration_name(), scxml.name, scxml.version, state_rows, transition_rows
        )
    };

    let revert = format!(
r#"-- Revert {}:{} from pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN


BEGIN;

with chart as (
    delete from fsm.statechart
    where name = '{}'
    and version = {}::semver
    returning id
)
delete from fsm.state
    where statechart_id = (select id from chart);

COMMIT;
"#,
        project, scxml.migration_name(), scxml.name, scxml.version
    );

    let verify = format!(
r#"-- Verify {}:{} on pg

-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN

BEGIN;

-- Verify that the statechart is added
select 1 / count(*)
from fsm.statechart
where
    name = '{}'
    and version = {}::semver;

-- Verify that the functions that the statechart depends on exist
do $$
declare
    missing_funcs_count_ int;
    missing_funcs_ text;
begin

select
  string_agg(distinct format('%s.%s', schema_name, function_name), ', '),
  count(*)
into
  missing_funcs_,
  missing_funcs_count_
from fsm.statechart
join fsm.state
    on state.statechart_id = statechart.id
, lateral unnest(on_entry || on_exit)
where
  statechart.name = '{}'
  and statechart.version = {}::semver
  and not exists (
    select 1
    from pg_proc p
    join pg_namespace n
      on p.pronamespace = n.oid
    where
      n.nspname = schema_name
      and p.proname = function_name
      and p.pronargs = 1
      and p.proargtypes[0] = 'fsm_event_payload'::regtype::oid
  );

if missing_funcs_count_ > 0 then
  raise exception
    $err$

    One or more missing or invalid functions: %
    All functions must take exactly one argument of the type fsm_event_payload

    $err$, missing_funcs_;
end if;

end
$$;

ROLLBACK;
"#,
        project, scxml.migration_name(), scxml.name, scxml.version, scxml.name, scxml.version
    );

    return Ok(Migration {
        deploy,
        revert,
        verify,
    });
}

// SQL SCXML representation
#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
struct SqlScxmlState {
    id: String,
    name: String,
    parent_id: Option<String>,
    is_initial: bool,
    is_final: bool,
    on_entry: Vec<(String, String)>,
    on_exit: Vec<(String, String)>,
}

#[derive(Debug, PartialEq)]
struct SqlScxmlTransition {
    event: String,
    source_state: String,
    target_state: String,
}

trait ToStatesAndTransitions {
    fn to_states_and_transitions(
        &self,
        parent_id: Option<String>,
        is_initial_state: &dyn Fn(&String) -> bool,
    ) -> Result<SqlStatesAndTransitions, String>;
}

impl ToStatesAndTransitions for State {
    fn to_states_and_transitions(
        &self,
        parent_id: Option<String>,
        is_initial_state: &dyn Fn(&String) -> bool,
    ) -> Result<SqlStatesAndTransitions, String>
    {
        let this_state = SqlScxmlState {
            id: self.id.clone(),
            name: self.name.clone(),
            parent_id: parent_id,
            is_initial: is_initial_state(&self.id),
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
            .to_states_and_transitions(Some(self.id.clone()), is_initial_state)?;

        let child_states = {
            let maybe_children_initial_id = self
                .initial
                .as_ref()
                .map(|i| i.transition.as_ref())
                .flatten()
                .map(|t| t.target.clone());

            // If there are child states then we also need to have an initial state for the child
            // states
            match (
                maybe_children_initial_id,
                self.child_states.is_empty()
            ) {
                (Some(children_initial_id), _) =>
                    self.child_states.to_states_and_transitions(
                        Some(self.id.clone()),
                        &|sid: &String| children_initial_id == *sid
                    )?,
                (None, true) => SqlStatesAndTransitions::new(),
                (None, false) => {
                    return Err(format!("state {} has child states but not initial", self.id));
                }
            }
        };

        let mut result = SqlStatesAndTransitions::new();
        result.states = vec![this_state];

        return Ok(result.join(transitions).join(child_states));
    }
}

impl ToStatesAndTransitions for FinalState {
    fn to_states_and_transitions(
        &self,
        parent_id: Option<String>,
        is_initial_state: &dyn Fn(&String) -> bool,
    ) -> Result<SqlStatesAndTransitions, String>

    {
        let this_state = SqlScxmlState {
            id: self.id.clone(),
            name: self.name.clone(),
            parent_id: parent_id,
            is_initial: is_initial_state(&self.id),
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

            // If there are child states then we also need to have an initial state for the child
            // states
            match (
                maybe_children_initial_id,
                self.child_states.is_empty()
            ) {
                (Some(children_initial_id), _) =>
                    self.child_states.to_states_and_transitions(
                        Some(self.id.clone()),
                        &|sid: &String| children_initial_id == *sid,
                    )?,
                (None, true) => SqlStatesAndTransitions::new(),
                (None, false) => {
                    return Err(format!("state {} has child states but not initial", self.id))
                }
            }
        };

        let mut result = SqlStatesAndTransitions::new();
        result.states = vec![this_state];

        return Ok(result.join(child_states));
    }
}

impl ToStatesAndTransitions for ParallelState {
    fn to_states_and_transitions(
        &self,
        parent_id: Option<String>,
        is_initial_state: &dyn Fn(&String) -> bool,
    ) -> Result<SqlStatesAndTransitions, String>

    {
        let this_state = SqlScxmlState {
            id: self.id.clone(),
            name: self.name.clone(),
            parent_id: parent_id,
            is_initial: is_initial_state(&self.id),
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
            .to_states_and_transitions(Some(self.id.clone()), is_initial_state)?;

        let child_states =
            self.child_states.to_states_and_transitions(
                Some(self.id.clone()),
                &|_: &String| true,
            )?;

        let mut result = SqlStatesAndTransitions::new();
        result.states = vec![this_state];

        return Ok(result.join(transitions).join(child_states));
    }
}

impl ToStatesAndTransitions for AbstractState {
    fn to_states_and_transitions(
        &self,
        parent_id: Option<String>,
        is_initial_state: &dyn Fn(&String) -> bool,
    ) -> Result<SqlStatesAndTransitions, String> {
        match self {
            AbstractState::State(state) => state.to_states_and_transitions(parent_id, is_initial_state),
            AbstractState::Final(state) => state.to_states_and_transitions(parent_id, is_initial_state),
            AbstractState::Parallel(state) => state.to_states_and_transitions(parent_id, is_initial_state)
        }
    }
}

impl ToStatesAndTransitions for AbstractStateWithoutFinal {
    fn to_states_and_transitions(
        &self,
        parent_id: Option<String>,
        is_initial_state: &dyn Fn(&String) -> bool,
    ) -> Result<SqlStatesAndTransitions, String> {
        match self {
            AbstractStateWithoutFinal::State(state) => state.to_states_and_transitions(parent_id, is_initial_state),
            AbstractStateWithoutFinal::Parallel(state) => state.to_states_and_transitions(parent_id, is_initial_state)
        }
    }
}


impl ToStatesAndTransitions for Transition {
    fn to_states_and_transitions(
        &self,
        parent_id: Option<String>,
        _is_initial_state: &dyn Fn(&String) -> bool,
    ) -> Result<SqlStatesAndTransitions, String>
    {
        let transition = match parent_id {
            None => return Err(format!("Can't have transition without parent_id")),
            Some(some_parent_id) => SqlScxmlTransition {
                event: self.event.clone(),
                source_state: some_parent_id.clone(),
                target_state: self.target.clone(),
            },
        };

        let mut result = SqlStatesAndTransitions::new();
        result.transitions = vec![transition];

        return Ok(result);
    }
}

impl<T> ToStatesAndTransitions for Vec<T>
where
    T: ToStatesAndTransitions,
{
    fn to_states_and_transitions(
        &self,
        parent_id: Option<String>,
        is_initial_state: &dyn Fn(&String) -> bool,
    ) -> Result<SqlStatesAndTransitions, String>

    {
        let mut result = SqlStatesAndTransitions::new();

        for item in self.iter() {
            let item_result = item.to_states_and_transitions(parent_id.clone(), is_initial_state)?;
            result = result.join(item_result);
        }

        Ok(result)
    }
}

// Raw SCXML representation (as represented in the XML)
#[derive(Debug, Deserialize, PartialEq)]
pub struct SCXML {
    #[serde(rename = "@xmlns")]
    pub xmlns: String,

    #[serde(rename = "@name", default)]
    pub name: String,

    #[serde(rename = "@version")]
    pub version: String,

    #[serde(rename = "@initial")]
    pub initial: String,

    #[serde(rename = "$value", default)]
    pub states: Vec<AbstractState>,
}

impl SCXML {
    pub fn migration_name(&self) -> String {
        format!(
            "statechart/{}-{}",
            self.name.replace(".", "/"),
            self.version
        )
    }
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum AbstractState {
    State(State),
    Final(FinalState),
    Parallel(ParallelState),
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum AbstractStateWithoutFinal {
    State(State),
    Parallel(ParallelState),
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct State {
    #[serde(rename = "@id")]
    pub id: String,

    #[serde(rename = "@name", default)]
    pub name: String,

    #[serde(rename = "transition", default)]
    pub transitions: Vec<Transition>,

    #[serde(rename = "onentry", default)]
    pub on_entry: Vec<OnEntry>,

    #[serde(rename = "onexit", default)]
    pub on_exit: Vec<OnExit>,

    #[serde(rename = "$value", default)]
    pub child_states: Vec<AbstractState>,

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

    #[serde(rename = "@name", default)]
    pub name: String,

    #[serde(rename = "onentry", default)]
    pub on_entry: Vec<OnEntry>,

    #[serde(rename = "$value", default)]
    pub child_states: Vec<AbstractState>,

    #[serde(rename = "initial", default)]
    pub initial: Option<Initial>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct ParallelState {
    #[serde(rename = "@id")]
    pub id: String,

    #[serde(rename = "@name", default)]
    pub name: String,

    #[serde(rename = "transition", default)]
    pub transitions: Vec<Transition>,

    #[serde(rename = "onentry", default)]
    pub on_entry: Vec<OnEntry>,

    #[serde(rename = "onexit", default)]
    pub on_exit: Vec<OnExit>,

    #[serde(rename = "$value", default)]
    pub child_states: Vec<AbstractStateWithoutFinal>,
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

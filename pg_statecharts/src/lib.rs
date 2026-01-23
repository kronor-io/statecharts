use pgrx::*;

mod gen_charts;
mod semver;


extension_sql_file!(
    "../sql/pg_statecharts--0.1.0--tables.sql",
    name = "tables",
    requires = ["semver_type"] // bootstrap
);

pgrx::pg_module_magic!();

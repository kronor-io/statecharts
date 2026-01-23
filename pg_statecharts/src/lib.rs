// src/lib.rs
use crate::pgrx_sql_entity_graph::metadata::*;
use pgrx::datum::PgVarlena;
use pgrx::*;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

mod gen_charts;
mod semver;

use std::str::FromStr;

/*
#[derive(Copy, Clone, Serialize, Deserialize, PostgresType)]
#[pgvarlena_inoutfuncs] // This is required for non-serde types
struct Semver {
    major: i32,
    minor: i32,
    patch: i32,
}

/// Implement the PgVarlenaInOutFuncs trait to provide our own text input and output functions
impl PgVarlenaInOutFuncs for Semver {
    // parse the provided CStr into a `PgVarlena<Semver>`
    fn input(input: &core::ffi::CStr) -> PgVarlena<Self> {
        let mut iter = input.to_str().unwrap().split('.');
        let (major, minor, patch) = (iter.next(), iter.next(), iter.next());

        let mut result = PgVarlena::<Semver>::new();
        result.major = i32::from_str(major.unwrap()).expect("major is not a valid i32");
        result.minor = i32::from_str(minor.unwrap()).expect("minor is not a valid i32");
        result.patch = i32::from_str(patch.unwrap()).expect("patch is not a valid i32");
        result
    }

    // Output ourselves as text into the provided `StringInfo` buffer
    fn output(&self, buffer: &mut StringInfo) {
        buffer.push_str(&format!("{}.{}.{}", self.major, self.minor, self.patch));
    }
}
*/

extension_sql_file!(
    "../sql/pg_statecharts--0.1.0--tables.sql",
    name = "tables",
    requires = ["semver_type"] // bootstrap
);

pgrx::pg_module_magic!();

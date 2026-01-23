use pgrx::datum::PgVarlena;
use pgrx::*;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::str::FromStr;

#[derive(Copy, Clone, Eq, PartialEq, Serialize, Deserialize, PostgresType)]
#[pgvarlena_inoutfuncs] // This is required for non-serde types
struct Semver {
    major: i32,
    minor: i32,
    patch: i32,
    // prerel: Option<String>,
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

impl PartialOrd for Semver {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Semver {
    fn cmp(&self, other: &Self) -> Ordering {
        self.major
            .cmp(&other.major)
            .then(self.minor.cmp(&other.minor))
            .then(self.patch.cmp(&other.patch))
    }
}
/*
#[pg_extern]
fn do_a_thing(mut input: PgVarlena<Semver>) -> PgVarlena<Semver> {
    input.c += 99; // performs a copy-on-write on the backing varlena pointer
    input
}
*/

#[pg_extern]
fn semver_lt(left: PgVarlena<Semver>, right: PgVarlena<Semver>) -> bool {
    left.as_ref() < right.as_ref()
}

#[pg_extern]
fn semver_le(left: PgVarlena<Semver>, right: PgVarlena<Semver>) -> bool {
    left.as_ref() <= right.as_ref()
}

#[pg_extern]
fn semver_eq(left: PgVarlena<Semver>, right: PgVarlena<Semver>) -> bool {
    left.as_ref() == right.as_ref()
}

#[pg_extern]
fn semver_ne(left: PgVarlena<Semver>, right: PgVarlena<Semver>) -> bool {
    left.as_ref() != right.as_ref()
}

#[pg_extern]
fn semver_ge(left: PgVarlena<Semver>, right: PgVarlena<Semver>) -> bool {
    left.as_ref() >= right.as_ref()
}

#[pg_extern]
fn semver_gt(left: PgVarlena<Semver>, right: PgVarlena<Semver>) -> bool {
    left.as_ref() > right.as_ref()
}

// Function needed for implicit casting of text to semver
#[pg_extern]
fn semver_from_text(input: &str) -> PgVarlena<Semver> {
    // Convert &str to CStr manually
    let cstr = std::ffi::CString::new(input).unwrap();
    Semver::input(cstr.as_c_str())
}

// B-tree comparison function (returns -1, 0, 1)
#[pg_extern]
fn semver_cmp(left: PgVarlena<Semver>, right: PgVarlena<Semver>) -> i32 {
    match left.as_ref().cmp(right.as_ref()) {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

// Hash function
#[pg_extern]
fn hash_semver(semver: PgVarlena<Semver>) -> i32 {
    // Simple but effective hash for 3 int32s
    // Using prime numbers for better distribution
    let mut hash: i32 = 17;
    hash = hash.wrapping_mul(31).wrapping_add(semver.major);
    hash = hash.wrapping_mul(31).wrapping_add(semver.minor);
    hash = hash.wrapping_mul(31).wrapping_add(semver.patch);
    hash
}

extension_sql_file!(
    "../sql/pg_statecharts--0.1.0--semver.sql",
    name = "semver_type",
    requires = [Semver, semver_lt, semver_ne, hash_semver]
);

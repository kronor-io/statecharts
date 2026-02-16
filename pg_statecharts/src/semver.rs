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
    patch: Option<i32>,
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
        result.patch = patch.map(|p| i32::from_str(p).expect("patch is not a valid i32"));
        result
    }

    // Output ourselves as text into the provided `StringInfo` buffer
    fn output(&self, buffer: &mut StringInfo) {
        let formatted_patch = match self.patch {
            Some(p) => format!(".{}", p),
            None => "".to_string(),
        };
        buffer.push_str(&format!("{}.{}{}", self.major, self.minor, formatted_patch));
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
    hash = hash
        .wrapping_mul(31)
        .wrapping_add(semver.patch.unwrap_or(0));
    hash
}

/*
extension_sql_file!(
    "../sql/pg_statecharts--0.1.0--semver.sql",
    name = "semver_type",
    requires = [Semver, semver_lt, semver_ne, hash_semver]
);
*/

extension_sql!(
    r#"
        create operator < (
            leftarg = semver,
            rightarg = semver,
            procedure = semver_lt,
            commutator = >,
            negator = >=,
            restrict = scalarltsel,
            join = scalarltjoinsel
        );
        
        create operator <= (
            leftarg = semver,
            rightarg = semver,
            procedure = semver_le,
            commutator = >=,
            negator = >,
            restrict = scalarltsel,
            join = scalarltjoinsel
        );
        
        create operator = (
            leftarg = semver,
            rightarg = semver,
            procedure = semver_eq,
            commutator = =,
            negator = <>,
            restrict = eqsel,
            join = eqjoinsel
        );
        
        create operator <> (
            leftarg = semver,
            rightarg = semver,
            procedure = semver_ne,
            commutator = <>,
            negator = =,
            restrict = neqsel,
            join = neqjoinsel
        );
        
        create operator >= (
            leftarg = semver,
            rightarg = semver,
            procedure = semver_ge,
            commutator = <=,
            negator = <,
            restrict = scalarltsel,
            join = scalarltjoinsel
        );
        
        create operator > (
            leftarg = semver,
            rightarg = semver,
            procedure = semver_gt,
            commutator = <,
            negator = <=,
            restrict = scalarltsel,
            join = scalarltjoinsel
        );
        
        --
        -- the btree indexing operator class.
        --
        
        create operator class semver_ops
            default for type semver using btree as
            operator    1   <  (semver, semver),
            operator    2   <= (semver, semver),
            operator    3   =  (semver, semver),
            operator    4   >= (semver, semver),
            operator    5   >  (semver, semver),
            function    1   semver_cmp(semver, semver);
        
        --
        -- the hash indexing operator class.
        -- note: you cannot have two operator classes with the same name!
        -- use a different name for hash operator class
        --
        
        create operator class semver_hash_ops
            default for type semver using hash as
            operator    1   =  (semver, semver),
            function    1   hash_semver(semver);
        
        create cast (text as semver)
        with function semver_from_text(text)
        as implicit;
        
        create or replace function to_semver(text_semver text)
          returns semver as
          $$
          declare
            dot_count integer;
          begin
            dot_count := length(text_semver) - length(replace(text_semver, '.', ''));
        
            -- pads with '.0' if the provided value has fewer than two dots
            return (text_semver || repeat('.0', 2 - dot_count))::semver;
          end;
          $$ language plpgsql;
    "#,
    name = "semver_operators",
    requires = [Semver, semver_lt, semver_ne, hash_semver]
);

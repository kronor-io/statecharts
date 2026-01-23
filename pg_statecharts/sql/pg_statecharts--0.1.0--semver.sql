--
-- comparison operators for semver type
--

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

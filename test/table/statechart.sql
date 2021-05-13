BEGIN;
  select plan(5);

  prepare insert_statechart as
    insert into fsm.statechart (name, version) values ($1, $2)
    returning id;

  select lives_ok($$execute insert_statechart('test', 1::semver)$$,
    'inserting name and version only should work'
  );

  select is(created_at, now(), 'created_at should have a default')
  from fsm.statechart
  limit 1;

  select throws_like($$execute insert_statechart('', 1::semver)$$,
    '%name_must_be_within_bounds%',
    'name should not be empty'
  );

  select throws_like($$execute insert_statechart('test', 1::semver)$$,
    '%idx_unique_name_version%',
    'name and version should be unique'
  );

  select lives_ok($$execute insert_statechart('test', 2::semver)$$,
    'inserting name and new version should work'
  );

  select finish();
ROLLBACK;

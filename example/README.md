This is a tiny example of a lightswitch statechart. There's a `lightswitch`
table that tracks when it was turned on and off and it has a state machine id
with a corresponding state machine.

The state machine has two states, on and off. 

### Setup

```bash
$ docker compose up -d db
[+] up 3/3
 ✔ Network example_default Created                                                                                                                                                  0.0s
 ✔ Volume example_dbdata   Created                                                                                                                                                  0.0s
 ✔ Container example-db-1  Created    
$
$
$ make deploy-migrations 
docker compose exec db sqitch --chdir sqitch deploy
Adding registry tables to db:postgres://postgres@localhost:5432/postgres
Deploying changes to db:postgres://postgres@localhost:5432/postgres
  + lightswitch ...................... psql:deploy/lightswitch.sql:7: NOTICE:  installing required extension "ltree"
psql:deploy/lightswitch.sql:7: NOTICE:  installing required extension "semver"
ok
  + statechart/lightswitch_flow-1.0 .. ok
$
$
$ make gen-charts 
docker compose exec db psql postgres://postgres:postgres@localhost:5432/postgres -c "$GEN_CHARTS_QUERY"
INFO:  deploy migration path: /repo/sqitch/deploy/statechart/lightswitch_flow-1.0.sql
 gen_statechart_sqitch_migrations 
----------------------------------
 
(1 row)

sudo chmod -R 777 sqitch/**/statechart
$
$ make deploy-migrations
```

### Using state machines

Once everything is set up you can access the database with `make
access-database` and insert into the lightswitch table and send events to the
corresponding state machine:

```sql
postgres=# insert into lightswitch default values;
INSERT 0 1
postgres=# 
postgres=# select * from lightswitch;
 id | state_machine_id | turned_on_at |         turned_off_at         
----+------------------+--------------+-------------------------------
  1 |                1 |              | 2026-01-27 19:18:30.885771+00
(1 row)

postgres=# 
postgres=# select fsm.notify_state_machine(1, state_machine_id, 'lightswitch.turn_on') from lightswitch;
 notify_state_machine 
----------------------
 
(1 row)

postgres=# 
postgres=# select * from lightswitch;
 id | state_machine_id |         turned_on_at          | turned_off_at 
----+------------------+-------------------------------+---------------
  1 |                1 | 2026-01-27 19:19:37.543633+00 | 
(1 row)
```


-- The generated sqitch migration bodies.
--
-- These are compared verbatim on purpose: the output has to stay byte for byte
-- what earlier versions produced, so that regenerating a chart does not show
-- up as a diff in the repository that stores it.

-- Error CONTEXT carries plpgsql line numbers, which would make this
-- expected output break on every unrelated edit.
\set SHOW_CONTEXT never
create extension if not exists pg_statecharts_dev cascade;

\set chart '<scxml xmlns="http://www.w3.org/2005/07/scxml" name="orders.checkout" version="1.2.3" initial="pending"><state id="pending" name="Pending"><transition event="order.pay" target="paying"/><onentry><script src="billing.reserve_stock"/><script src="notify_pending"/></onentry><onexit><script src="billing.release_hold"/></onexit></state><state id="paying" name="Paying"><initial><transition target="authorizing"/></initial><state id="authorizing" name="Authorizing"/></state><final id="done" name="Done"/></scxml>'

-- dots in the chart name become directories
select fsm.__migration_name('orders.checkout', '1.2.3'), fsm.__migration_name('flat', '2.0');

\echo '--- deploy ---'
select fsm.__migration_deploy(:'chart'::xml, 'myproject', 'orders.checkout', '1.2.3');

\echo '--- revert ---'
select fsm.__migration_revert('myproject', 'orders.checkout', '1.2.3');

\echo '--- verify ---'
select fsm.__migration_verify('myproject', 'orders.checkout', '1.2.3');

-- Values are quoted rather than interpolated raw, so a quote in a chart name
-- cannot break out of the generated SQL.
select fsm.__migration_revert('proj', 'it''s', '1.0.0') ~ 'name = ''it''''s''' as name_is_escaped;

select fsm.__callback_literals(array[]::fsm_callback_name[]) = '' as empty_renders_empty;
select fsm.__callback_literals(array[('a','b'),('c','d')]::fsm_callback_name[]);

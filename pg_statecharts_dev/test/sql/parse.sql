-- Parsing SCXML into states and transitions.

-- Error CONTEXT carries plpgsql line numbers, which would make this
-- expected output break on every unrelated edit.
\set SHOW_CONTEXT never
create extension if not exists pg_statecharts_dev cascade;

\set chart '<scxml xmlns="http://www.w3.org/2005/07/scxml" name="orders.checkout" version="1.2.3" initial="pending"><state id="pending" name="Pending"><transition event="order.pay" target="paying"/><transition event="order.cancel" target="cancelled"/><onentry><script src="billing.reserve_stock"/><script src="notify_pending"/></onentry><onexit><script src="billing.release_hold"/></onexit></state><state id="paying" name="Paying"><initial><transition target="authorizing"/></initial><transition event="order.paid" target="fulfilling"/><state id="authorizing" name="Authorizing"><transition event="auth.ok" target="capturing"/></state><state id="capturing" name="Capturing"/></state><parallel id="fulfilling" name="Fulfilling"><transition event="order.done" target="complete"/><state id="packing" name="Packing"/><state id="invoicing" name="Invoicing"><initial><transition target="drafting"/></initial><state id="drafting" name="Drafting"/><state id="sent" name="Sent"/></state></parallel><final id="complete" name="Complete"><onentry><script src="audit.log_complete"/></onentry><onexit><script src="never.called"/></onexit></final><final id="cancelled" name="Cancelled"/></scxml>'

-- States come out in document order, with parent_id and is_initial resolved
-- from the enclosing <initial> element. Every child of a <parallel> is initial.
select id, name, parent_id, is_initial, is_final, on_entry::text, on_exit::text
from fsm.scxml_states(:'chart'::xml);

-- Transitions belong to the state that encloses them. The <initial>
-- transitions and anything under a <final> must not appear here.
select event, source_state, target_state from fsm.scxml_transitions(:'chart'::xml);

-- A dotted src is split on the first dot, an undotted one defaults to public
select (fsm.scxml_states(
  '<scxml xmlns="http://www.w3.org/2005/07/scxml" name="c" version="1.0" initial="s">
     <state id="s"><onentry>
       <script src="plain"/><script src="a.b"/><script src="a.b.c"/>
     </onentry></state>
   </scxml>'::xml)).on_entry::text as callbacks;

-- XML entities in attributes have to be decoded, not passed through escaped
select id, name from fsm.scxml_states(
  '<scxml xmlns="http://www.w3.org/2005/07/scxml" name="c" version="1.0" initial="s">
     <state id="s" name="Pick &amp; Pack &lt;fast&gt;"/>
   </scxml>'::xml);

-- Namespaceless documents are accepted too
select id from fsm.scxml_states(
  '<scxml name="c" version="1.0" initial="s"><state id="s"/></scxml>'::xml);

-- An unnamed state gets an empty name rather than being rejected
select id, name = '' as name_is_empty from fsm.scxml_states(
  '<scxml name="c" version="1.0" initial="s"><state id="s"/></scxml>'::xml);

--
-- Error cases
--

-- compound state without an <initial>
select fsm.scxml_states(
  '<scxml name="c" version="1.0" initial="p">
     <state id="p"><state id="c1"/><state id="c2"/></state>
   </scxml>'::xml);

-- <initial> pointing at a state that is not a child
select fsm.scxml_states(
  '<scxml name="c" version="1.0" initial="p">
     <state id="p"><initial><transition target="nope"/></initial><state id="c1"/></state>
   </scxml>'::xml);

-- root initial pointing at nothing
select fsm.scxml_states(
  '<scxml name="c" version="1.0" initial="nope"><state id="s"/></scxml>'::xml);

-- state without an id
select fsm.scxml_states(
  '<scxml name="c" version="1.0" initial="s"><state id="s"/><state name="oops"/></scxml>'::xml);

-- no states at all
select fsm.scxml_states('<scxml name="c" version="1.0" initial="s"/>'::xml);

-- no initial attribute
select fsm.scxml_states('<scxml name="c" version="1.0"><state id="s"/></scxml>'::xml);

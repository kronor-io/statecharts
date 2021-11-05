module SQLTestGenSpec where

import Statechart.Analysis
import Statechart.Types
import Data.String.Interpolate
import RIO
import Test.Hspec
import Statechart.SCXML (parse)
import Statechart.CodeGen.SQLTests
import RIO.ByteString.Lazy qualified as Lazy

spec :: Spec
spec = do
      describe "manually written pipeline" $ do
            let parsed = case parse manualSCXML of -- Either Text (Chart StateName EventName)
                  Left _ -> undefined
                  Right r -> r
            let sqlTest = genTest . mkTest "schema_name_flow" "schema_name" $ parsed
            it "should generate what we expected" $ sqlTest `shouldBe` expectedSQL

manualSCXML :: Lazy.ByteString
manualSCXML = [i|
<scxml xmlns="http://www.w3.org/2005/07/scxml"
              version="1.0"
              initial="initial_state_top">
    <state id="initial_state_top" name="">
        <initial>
            <transition target="initial_state_top_child"/>
        </initial>
        <state id="initial_state_top_child" name="">
            <initial>
                <transition target="initial_state"/>
            </initial>
            <state id="initial_state" name="">
                <transition event="schema_name.time.due" target="due_date"/>
                <transition event="schema_name.time.soft_reminder" target="in_progress_soft_reminder"/>
                <onentry>
                    <script src="schema_name.action01" />
                </onentry>
            </state>
            <state id="due_date" name="">
                <transition event="schema_name.time.reminder1" target="reminder1"/>
                <onentry>
                    <script src="schema_name.action02" />
                </onentry>
            </state>
            <state id="reminder1" name="">
                <transition event="schema_name.time.reminder2" target="reminder2"/>
                <onentry>
                    <script src="schema_name.action03" />
                </onentry>
            </state>
            <state id="reminder2" name="">
                <transition event="schema_name.time.debt_collection" target="debt_collection_date"/>
                <onentry>
                    <script src="schema_name.action04" />
                </onentry>
            </state>
            <final id="debt_collection_date" name="">
                <onentry>
                    <script src="schema_name.action05" />
                </onentry>
            </final>
            <state id="in_progress_soft_reminder" name="">
                <transition event="schema_name.time.due" target="due_date"/>
                <onentry>
                    <script src="schema_name.action06" />
                </onentry>
            </state>
            <transition event="schema_name.pay.enough" target="minimum_payment"/>
        </state>
        <state id="minimum_payment" name="">
            <initial>
                <transition target="paid_more_than_min"/>
            </initial>
            <state id="paid_more_than_min" name="">
                <onentry>
                    <script src="schema_name.action07" />
                </onentry>
                <transition event="schema_name.time.soft_reminder" target="minimum_payment_soft_reminder"/>
                <transition event="schema_name.time.due" target="transferring_to_account"/>
                <transition event="schema_name.time.past_due" target="transferring_to_account"/>
            </state>
            <state id="minimum_payment_soft_reminder" name="">
                <transition event="schema_name.time.due" target="transferring_to_account"/>
                <onentry>
                    <script src="schema_name.action03" />
                </onentry>
            </state>
            <final id="transferring_to_account" name="">
                <onentry>
                    <script src="schema_name.action08" />
                </onentry>
            </final>
        </state>
        <transition event="done.state.debt_collection_date" target="debt_collection"/>
        <transition event="done.state.minimum_payment" target="transferred_to_account"/>
        <transition event="schema_name.settle" target="settled"/>
    </state>
    <final id="debt_collection" name="">
    </final>
    <final id="transferred_to_account" name="">
    </final>
    <final id="settled" name="">
        <onentry>
            <script src="schema_name.action09" />
        </onentry>
    </final>
</scxml>
|]

expectedSQL :: Text
expectedSQL = [i|
-----------------------------------------------------------------------------------------------
--                                                                                           --
--                                        -----------                                        --
--                                        - WARNING -                                        --
--                                        -----------                                        --
--                                                                                           --
--                                            ***                                            --
--                                                                                           --
--                                   DO NOT EDIT THIS FILE                                   --
--                               IT WAS AUTOMATICALLY GENERATED                              --
--                                CHANGES MAY BE OVERWRITTEN                                 --
--                                                                                           --
--                                            ***                                            --
--                                                                                           --
-----------------------------------------------------------------------------------------------
BEGIN;
select plan(24); -- (PG_TAP function)
\\i statecharts/test/setup_helpers.sql
-----------------------------------------------------------------------------------------------
-- FUNCTION CHECKS ----------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
select is(function_exists('schema_name','action01'), true, 'action01');
select is(function_exists('schema_name','action02'), true, 'action02');
select is(function_exists('schema_name','action03'), true, 'action03');
select is(function_exists('schema_name','action04'), true, 'action04');
select is(function_exists('schema_name','action05'), true, 'action05');
select is(function_exists('schema_name','action06'), true, 'action06');
select is(function_exists('schema_name','action07'), true, 'action07');
select is(function_exists('schema_name','action08'), true, 'action08');
-----------------------------------------------------------------------------------------------
-- INTERCEPTIONS ------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
create or replace function schema_name.action01(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action01'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action02(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action02'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action02(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action02'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action03(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action03'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action04(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action04'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action05(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action05'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action06(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action06'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action07(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action07'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action08(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action08'); return; end; $$ language plpgsql volatile strict;
-----------------------------------------------------------------------------------------------
-- TRANSITIONS TESTS --------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
select is((select fsm.is_state_active(:shard,:mid,'initial_state')),true, 'state is created');
select is((select last_intercepted()),'schema_name.created_action');
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
select fsm.notify_state_machine(:shard,:mid,'schema_name.time.soft_reminder');
select is((select fsm.is_state_active(:shard,:mid,'in_progress_soft_reminder')),true, 'state is in_progress_soft_reminder');
select is((select last_intercepted()),'schema_name.soft_reminder_action');
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
update fsm.state_machine_state SET state_id = 'in_progress_soft_reminder' where state_machine_id = :mid and shard_id = :shard and state_id = 'initial_state';
select fsm.notify_state_machine(:shard,:mid,'schema_name.time.due');
select is((select fsm.is_state_active(:shard,:mid,'due_date')),true, 'state is due_date');
select is((select last_intercepted()),'schema_name.due_date_action');
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
update fsm.state_machine_state SET state_id = 'due_date' where state_machine_id = :mid and shard_id = :shard and state_id = 'initial_state';
select fsm.notify_state_machine(:shard,:mid,'schema_name.time.reminder1');
select is((select fsm.is_state_active(:shard,:mid,'reminder1')),true, 'state is reminder1');
select is((select last_intercepted()),'schema_name.reminder1_action');
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
update fsm.state_machine_state SET state_id = 'reminder1' where state_machine_id = :mid and shard_id = :shard and state_id = 'initial_state';
select fsm.notify_state_machine(:shard,:mid,'schema_name.time.reminder2');
select is((select fsm.is_state_active(:shard,:mid,'reminder2')),true, 'state is reminder2');
select is((select last_intercepted()),'schema_name.reminder2_action');
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
update fsm.state_machine_state SET state_id = 'reminder2' where state_machine_id = :mid and shard_id = :shard and state_id = 'initial_state';
select fsm.notify_state_machine(:shard,:mid,'schema_name.time.debt_collection');
select is((select fsm.is_state_active(:shard,:mid,'debt_collection_date')),true, 'state is debt_collection_date');
select is((select last_intercepted()),'schema_name.debt_collection_action');
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
update fsm.state_machine_state SET state_id = 'debt_collection_date' where state_machine_id = :mid and shard_id = :shard and state_id = 'initial_state';
select fsm.notify_state_machine(:shard,:mid,'schema_name.pay.enough');
select is((select fsm.is_state_active(:shard,:mid,'paid_more_than_min')),true, 'state is paid_more_than_min');
select is((select last_intercepted()),'schema_name.paid_more_than_min_action');
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
update fsm.state_machine_state SET state_id = 'paid_more_than_min' where state_machine_id = :mid and shard_id = :shard and state_id = 'initial_state';
select fsm.notify_state_machine(:shard,:mid,'schema_name.time.soft_reminder');
select is((select fsm.is_state_active(:shard,:mid,'minimum_payment_soft_reminder')),true, 'state is minimum_payment_soft_reminder');
select is((select last_intercepted()),'schema_name.soft_reminder_action');
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
update fsm.state_machine_state SET state_id = 'minimum_payment_soft_reminder' where state_machine_id = :mid and shard_id = :shard and state_id = 'initial_state';
select fsm.notify_state_machine(:shard,:mid,'schema_name.time.due');
select is((select fsm.is_state_active(:shard,:mid,'transferring_to_account')),true, 'state is transferring_to_account');
select is((select last_intercepted()),'schema_name.transferring_to_account_action');
select id as mid from fsm.start_machine_with_latest_statechart(:shard,:chartname) \\gset
update fsm.state_machine_state SET state_id = 'transferring_to_account' where state_machine_id = :mid and shard_id = :shard and state_id = 'initial_state';
select fsm.notify_state_machine(:shard,:mid,'schema_name.settle');
select is((select fsm.is_state_active(:shard,:mid,'settled')),true, 'state is transferring_to_account');
select is((select last_intercepted()),'schema_name.settled_action');
-----------------------------------------------------------------------------------------------
-- FINISHING ----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
select finish(); -- (PG_TAP function)
ROLLBACK;
-----------------------------------------------------------------------------------------------
-- END ----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
|]

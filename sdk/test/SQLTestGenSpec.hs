module SQLTestGenSpec where

import Statechart.Types
import Data.String.Interpolate
import RIO
import Test.Hspec
import Statechart.SCXML (parse)
import Statechart.CodeGen.SQLTests
import RIO.ByteString.Lazy qualified as Lazy
import RIO.Text qualified as T

spec :: Spec
spec = do
      describe "manually written pipeline" $ do
            let parsed = case parse manualSCXML of -- Either Text (Chart StateName EventName)
                  Left _ -> undefined
                  Right r -> r
            let sqlTest = genTest . mkTest "someflow" "schema_name" $ parsed
            --it "should generate what we expected" $ T.lines sqlTest `shouldBe` T.lines expectedSQL
            it "should generate what we expected" $ sqlTest `shouldBe` expectedSQL

manualSCXML :: Lazy.ByteString
manualSCXML = [i|
<scxml xmlns="http://www.w3.org/2005/07/scxml"
              version="1.0"
              initial="initial_state">
    <state id="initial_state" name="">
        <transition event="event01" target="state02"/>
        <transition event="event02" target="state03"/>
        <onentry>
             <script src="schema_name.action01" />
        </onentry>
    </state>
    <state id="state02" name="">
        <transition event="event03" target="state03"/>
        <onentry>
             <script src="schema_name.action02" />
        </onentry>
    </state>
    <state id="state03" name="">
        <transition event="event04" target="state04"/>
    </state>
    <final id="state04" name="">
        <onentry>
             <script src="schema_name.action03" />
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
select plan(7); -- (PG_TAP function)
\\i statecharts/test/setup_helpers.sql
-----------------------------------------------------------------------------------------------
-- FUNCTION CHECKS ----------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
select is(function_exists('schema_name','action01'), true);
select is(function_exists('schema_name','action02'), true);
select is(function_exists('schema_name','action03'), true);
-----------------------------------------------------------------------------------------------
-- INTERCEPTIONS ------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
create or replace function schema_name.action01(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action01'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action02(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action02'); return; end; $$ language plpgsql volatile strict;
create or replace function schema_name.action03(event_payload fsm_event_payload) returns void as $$ begin perform intercepted_('schema_name.action03'); return; end; $$ language plpgsql volatile strict;
-----------------------------------------------------------------------------------------------
-- TRANSITIONS TESTS --------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
select id as mid from fsm.start_machine_with_latest_statechart(1,'someflow') \\gset
select is((select fsm.is_state_active(1,:mid,'initial_state')),true);
select is((select last_intercepted()),'schema_name.action01');

select id as mid from fsm.start_machine_with_latest_statechart(1,'someflow') \\gset
select fsm.notify_state_machine(1,:mid,'event01');
select is((select fsm.is_state_active(1,:mid,'state02')),true);
select is((select last_intercepted()),'schema_name.action02');

select id as mid from fsm.start_machine_with_latest_statechart(1,'someflow') \\gset
select fsm.notify_state_machine(1,:mid,'event02');
select is((select fsm.is_state_active(1,:mid,'state04')),true);
select is((select last_intercepted()),'schema_name.action03');

select id as mid from fsm.start_machine_with_latest_statechart(1,'someflow') \\gset
update fsm.state_machine_state SET state_id = 'state02' where state_machine_id = :mid and shard_id = 1 and state_id = 'initial_state';
select fsm.notify_state_machine(1,:mid,'event03');
select is((select fsm.is_state_active(1,:mid,'state03')),true);

select id as mid from fsm.start_machine_with_latest_statechart(1,'someflow') \\gset
update fsm.state_machine_state SET state_id = 'state03' where state_machine_id = :mid and shard_id = 1 and state_id = 'initial_state';
select fsm.notify_state_machine(1,:mid,'event04');
select is((select fsm.is_state_active(1,:mid,'state04')),true);
select is((select last_intercepted()),'schema_name.action03');

-----------------------------------------------------------------------------------------------
-- FINISHING ----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
select finish(); -- (PG_TAP function)
ROLLBACK;
-----------------------------------------------------------------------------------------------
-- END ----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
|]

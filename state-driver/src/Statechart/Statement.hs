module Statechart.Statement where

import Data.Vector qualified
import Hasql.Decoders qualified
import Hasql.Encoders
import Hasql.Statement qualified
import Relude
import Statechart

handleMachineEvents :: Hasql.Statement.Statement StateMachine ()
handleMachineEvents =
    Hasql.Statement.Statement
        "select 1::bigint from fsm.handle_machine_events($1::bigint, $2::bigint)"
        toParams
        Hasql.Decoders.noResult
        False

getPendingMachines :: Hasql.Statement.Statement () (Data.Vector.Vector StateMachine)
getPendingMachines =
    Hasql.Statement.Statement
        "select distinct shard_id::bigint, state_machine_id::bigint from fsm.state_machine_event where handled_at is null"
        mempty
        (Hasql.Decoders.rowVector stateMachineRow)
        False

toParams :: Params StateMachine
toParams = shardParam <> machineParam

shardParam :: Params StateMachine
shardParam = shard >$< param (nonNullable int8)

machineParam :: Params StateMachine
machineParam = machine >$< param (nonNullable int8)

shard :: StateMachine -> Int64
shard StateMachine{shardId = (Id (Shard s))} = s

machine :: StateMachine -> Int64
machine StateMachine{stateMachineId = (Id (Machine m))} = m

stateMachineRow :: Hasql.Decoders.Row StateMachine
stateMachineRow =
    StateMachine
        <$> fmap coerce idColumn
        <*> fmap coerce idColumn

idColumn :: Hasql.Decoders.Row Int64
idColumn = Hasql.Decoders.column . Hasql.Decoders.nonNullable $ Hasql.Decoders.int8

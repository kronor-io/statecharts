module Statechart.Transaction where

import Data.Vector qualified
import Hasql.Transaction qualified
import Statechart (StateMachine)
import Statechart.Statement qualified as Statement

handleMachineEvents :: StateMachine -> Hasql.Transaction.Transaction ()
handleMachineEvents stateMachine =
    Hasql.Transaction.statement stateMachine Statement.handleMachineEvents

getPendingMachines :: Hasql.Transaction.Transaction (Data.Vector.Vector StateMachine)
getPendingMachines =
    Hasql.Transaction.statement () Statement.getPendingMachines

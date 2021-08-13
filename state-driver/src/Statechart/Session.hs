module Statechart.Session where

import Data.Vector qualified
import Hasql.Session qualified
import Hasql.Transaction.Sessions qualified as Hasql.Transaction
import Statechart (StateMachine)
import Statechart.Transaction qualified

handleMachineEvents :: StateMachine -> Hasql.Session.Session ()
handleMachineEvents stateMachine =
    Hasql.Transaction.transaction
        Hasql.Transaction.Serializable
        Hasql.Transaction.Write
        (Statechart.Transaction.handleMachineEvents stateMachine)

getPendingMachines :: Hasql.Session.Session (Data.Vector.Vector StateMachine)
getPendingMachines =
    Hasql.Transaction.transaction
        Hasql.Transaction.ReadCommitted
        Hasql.Transaction.Read
        Statechart.Transaction.getPendingMachines

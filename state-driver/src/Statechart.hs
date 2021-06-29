module Statechart where

import Data.Aeson qualified as Json
import Data.Hashable (Hashable)
import Data.Kind (Constraint)
import Deriving.Aeson.Stock qualified as Json
import Relude

newtype Id a = Id a
  deriving stock (Show)
  deriving newtype (Eq, Hashable, Json.FromJSON)

newtype Shard = Shard Int64
  deriving stock (Show)
  deriving newtype (Eq, Hashable, Json.FromJSON)

newtype Machine = Machine Int64
  deriving stock (Show)
  deriving newtype (Eq, Hashable, Json.FromJSON)

data StateMachine = StateMachine
  { shardId :: Id Shard,
    stateMachineId :: Id Machine
  }
  deriving stock (Eq, Show, Generic)
  deriving (Json.FromJSON) via Json.Snake StateMachine

instance Hashable StateMachine

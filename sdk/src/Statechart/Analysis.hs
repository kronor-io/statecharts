module Statechart.Analysis where

import RIO
import RIO.Set qualified as S
import Statechart.Types
import Control.Monad.State qualified as State
import Data.Foldable
import Statechart.Helpers (getAllTransitions)


data TracingState = TracingState
  { originalChart  :: Chart StateName EventName
  , currentState   :: Text
  , currentPath    :: Path
  , alreadyVisited :: Set StateName
  , paths          :: Set Path
  }

data Path
  = Nil
  | State Text Path
  | Trans Text Path

appe :: Path -> Path -> Path
appe = undefined

runTracing :: State.MonadState TracingState m => m a -> IO a
runTracing = undefined

getPaths :: State.MonadState TracingState m => Chart StateName EventName -> m (Set Path)
getPaths chart = do
  iname_  <- State.gets (initial . originalChart)
  states_ <- State.gets (f iname_ . states . originalChart)
  State.modify (\t -> t { alreadyVisited = S.insert iname_ (alreadyVisited t)})
  let ts :: [Transition StateName EventName] = getAllTransitions (g iname_ states_)
  foldrM fromInitial S.empty ts
 where
  f :: StateName -> [State StateName EventName] -> Int
  f = undefined
  g :: StateName -> Int -> State StateName EventName
  g = undefined
  -- this function starts from the initial state and then start brancing
  fromInitial :: Transition StateName EventName -> Set Path -> m (Set Path)
  fromInitial = undefined

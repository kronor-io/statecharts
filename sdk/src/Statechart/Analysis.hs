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

-- this can be done in a bunch of ways, this seems like the most simple
-- alternatively the states might be implicit, but this goes against the plan of extending the scxmls in the future, in a way, having the states in here is the first constraint that we can put on a path.
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
  -- this function starts from the initial state and then goes brancing
  -- TODO but i believe the state can be modified, already visited is not to work as previiously exepcted
  fromInitial :: Transition StateName EventName -> Set Path -> m (Set Path)
  fromInitial = undefined


-- TODO alternatively we might use the bimapState function. How can it be used?
-- maybe enrich and then contract? nah, there should be an easier way.
-- maybe charts can be made traversable?


-- TODO since this is the kind of function that is easy to get wrong, invest into good unit test of smaller functions, thats the way to go, I have proclaimed
-- property tests would be nice

-- NOTES FOR FUTURE SELF

-- the reason the field alreadyVisited wont work as is is that we sometimes still want to go to a state more than once if the previous state is different, if it is a loop then we dont want, but if its not a loop we want, it likes a number were only the first digit is different:
-- 234512134
-- 434512134

-- TODO also bear in mind that *IF* we extend the scxmls, between paths there will also be some checks, like specific values that should be set in cat we have traversed that state

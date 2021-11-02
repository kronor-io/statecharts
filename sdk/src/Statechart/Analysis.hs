module Statechart.Analysis (getPaths, Path, Hop(..)) where

import Control.Monad.State (StateT, evalState, gets)
import Data.Foldable
import RIO
import RIO.Set qualified as S
import Statechart.Helpers
import Statechart.Types

------------
-- PUBLIC --
------------

getPaths :: Chart StateName EventName -> Set Path
getPaths chart = evalState getPathsT (tracingChart chart)
  where
    getPathsT :: StateT TracingState Identity (Set Path)
    getPathsT = do
        initialState <- gets (getInitialState . originalChart)
        foldrM fromInitial S.empty (getAllTransitions initialState)

-------------
-- PRIVATE --
-------------

type Path = [Hop]

data Hop = Hop
  { transition  :: Maybe EventName
  , targetState :: StateName
  , actions     :: [Text]
  }

data TracingState = TracingState
    { originalChart :: Chart StateName EventName
    , currentState :: StateName
    , currentPath :: Path
    , alreadyVisited :: Set StateName
    , paths :: Set Path
    }

-- | Creates a new state over the given chart.
tracingChart :: Chart StateName EventName -> TracingState
tracingChart chart =
    TracingState
        { originalChart = chart
        , currentState = sid (getInitialState chart)
        , currentPath = []
        , alreadyVisited = S.fromList [sid (getInitialState chart)]
        , paths = S.empty
        }

fromInitial :: Transition StateName EventName -> Set Path -> StateT TracingState Identity (Set Path)
fromInitial trans acc = undefined

-- TODO alternatively we might use the bimapState function.
-- How can it be used?
-- maybe enrich and then contract? nah, there should be an
-- easier way.
-- maybe charts can be made traversable?

-- TODO since this is the kind of function that is easy to
-- get wrong, invest into good unit test of smaller functions,
-- thats the way to go, I have proclaimed
-- property tests would be nice

-- NOTES FOR FUTURE SELF

-- the reason the field alreadyVisited wont work as is is
-- that we sometimes still want to go to a state more than
-- once if the previous state is different, if it is a loop
-- then we dont want, but if its not a loop we want, it likes
-- a number were only the first digit is different:
-- 234512134
-- 434512134

-- TODO also bear in mind that *IF* we extend the scxmls,
-- between paths there will also be some checks, like
-- specific values that should be set in cat we have
-- traversed that state

-- this can be done in a bunch of ways, this seems like the most simple
-- alternatively the states might be implicit, but this goes against the
-- plan of extending the scxmls in the future, in a way, having the states
-- in here is the first constraint that we can put on a path.

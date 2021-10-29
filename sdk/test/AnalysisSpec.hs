module AnalysisSpec (spec) where

import Helper
import Plugin.Haskell.Chart1
import Plugin.Haskell.Chart2
import Plugin.Haskell.Chart3
import RIO.ByteString qualified as BS
import RIO.Map qualified as M
import RIO.Text qualified as T
import Statechart.CodeGen.SQL qualified as SQL
import System.Directory
import Control.Monad.State
import Statechart.Analysis

spec :: Spec
spec = do
  -- TODO we want a bunch of unit tests
  -- getting the paths out of the chart is that kind of tricky functiion with a funch of maps and folds that 
  -- get confusing quickly, so make sure to go incrmentally and using unit tests for small functions.
  return ()

-------------
-- HELPERS --
-------------

shouldReturn' :: (Show a, Eq a, MonadState TracingState m) => m a -> a -> Expectation
shouldReturn' ma expected'a = runTracing ma `shouldReturn` expected'a

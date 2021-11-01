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
  getPathsSpec

getPathsSpec :: Spec
getPathsSpec = do
  describe "getPaths" $ do
    it "" pending
    it "" pending
    it "" pending
  describe "fromInitial" $ do
    it "" pending
    it "" pending
    it "" pending

-------------
-- HELPERS --
-------------

-- not needed anymore i think
------------------shouldReturn' :: (Show a, Eq a, MonadState TracingState m) => m a -> a -> Expectation
------------------shouldReturn' ma expected'a = runTracing ma `shouldReturn` expected'a

-- | This module contains functions for going from charts to sql files.
-- Eventually it should be done with pretty printers to make it more robust (also pretty)
module Statechart.CodeGen.SQLTests(mkTest,genTest) where

import Data.Text as T
import RIO
import Statechart.Types
import GHC.RTS.Flags (ProfFlags(includeTSOs))

------------
-- PUBLIC --
------------

mkTest :: Chart StateName EventName -> SQLTest
mkTest = undefined

genTest :: SQLTest -> Text
genTest = undefined

-------------
-- HELPERS --
-------------

data SQLTest = SQLTest
  { chartname     :: Text
  , actions       :: [Text]
  , interceptions :: [Text]
  , tests         :: [IndividualTest]
  }

data IndividualTest = IndividualTest
  { source    :: Text
  , transname :: Text
  , target    :: Text
  , on_entry  :: [Text]
  }

-- | The total number of tests we intended to run.
planNumber :: SQLTest -> Int
planNumber = undefined

-- | Generate the most part of the static stuff at the top, but includes the plan and the import of the functions file.
genHeader :: Int -> Text
genHeader = undefined

-- | This add static checks for the action functions. Mostly a courtesy.
genFnCheck :: Text -> Text
genFnCheck = undefined

-- | Used to generate a new definition of the action function so we can intercept its instead of letting it run.
genInterception :: Text -> Text
genInterception = undefined

-- | We use this to generate the each cluster of a transition test, which includes several lines.
genTransitionTest :: IndividualTest -> Text
genTransitionTest = undefined

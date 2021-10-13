{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Plugin.Haskell.Chart1 where

import Data.FileEmbed
import Language.Haskell.TH.Syntax (liftString)
import RIO
import Statechart.CodeGen.Haskell
import Statechart.Types

genCodeFromFile
    "chart1"
    $(makeRelativeToProject "test/Plugin/SCXML/chart1.scxml" >>= liftString)

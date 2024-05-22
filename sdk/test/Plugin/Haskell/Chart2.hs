{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}
{-# OPTIONS_GHC -w #-}

module Plugin.Haskell.Chart2 where

import Data.FileEmbed
import Language.Haskell.TH.Syntax (liftString)
import RIO
import Statechart.CodeGen.Haskell
import Statechart.Types

genCodeFromFile
    "chart2"
    $(makeRelativeToProject "test/Plugin/SCXML/chart2.scxml" >>= liftString)

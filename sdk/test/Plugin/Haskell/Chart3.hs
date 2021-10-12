{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Plugin.Haskell.Chart3 where

import Data.FileEmbed
import Language.Haskell.TH.Syntax (liftString)
import RIO
import Statechart.CodeGen.Haskell
import Statechart.Types

genCodeFromFile
    "chart3"
    $(makeRelativeToProject "test/Plugin/SCXML/chart3.scxml" >>= liftString)

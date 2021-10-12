{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Plugin.Haskell.PurchaseFlow where

import Data.FileEmbed
import Language.Haskell.TH.Syntax (liftString)
import RIO
import Statechart.CodeGen.Haskell
import Statechart.Types

genCodeFromFile
    "purchase_flow"
    $(makeRelativeToProject "test/Plugin/SCXML/purchase_flow.scxml" >>= liftString)

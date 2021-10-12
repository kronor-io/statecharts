{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Plugin.Haskell.InvoiceFlow where

import Data.FileEmbed
import Language.Haskell.TH.Syntax (liftString)
import Haskell
import RIO
import Types

genCodeFromFile "invoice_flow"
    $(makeRelativeToProject "test/Plugin/SCXML/invoice_flow.scxml" >>= liftString)

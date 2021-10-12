{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Plugin.Haskell.PaymentFlow where

import Data.FileEmbed
import Language.Haskell.TH.Syntax (liftString)
import Haskell
import RIO
import Types

genCodeFromFile "payment_flow"
    $(makeRelativeToProject "test/Plugin/SCXML/payment_flow.scxml" >>= liftString)

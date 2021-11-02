-- | This module contains functions for going from charts to sql files.
-- Eventually it should be done with pretty printers to make it more robust (also pretty)
module Statechart.CodeGen.SQLTests where

import Data.String.Interpolate (i, iii)
import Data.Text as T
import RIO
import RIO.ByteString qualified as BS
import RIO.Text qualified as T
import Statechart.Helpers
import Statechart.Types
import System.FilePath.Posix (dropExtension)



  {-
in here there will be logic for gathering the information
from the analysis and use it to generate the
notify_state_machine sequences together we the redefinition
of the function fsm.handle_state_events (or something like
that) that I checked and it worked.
-}




-- given a path, we can generate a sql test for it
--genTest :: Path -> Text
--genTest = undefined



--layoutNotifyStateMachine = [iii| select fsm.notify_state_machine...etc |]

--layoutReplaceEventHandler = [iii| create or replace function fsm.handle_machine_events...etc |]

-- TODO consider how similar the generation of SQL tests is
--to what we already have in terms of SQL generation, because
--we generate the sql charts. So should they be a single
--helper module and this kind of thing to make things smaller
--and more robust?

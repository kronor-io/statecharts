module Statechart.Types where

import Data.Aeson
import RIO
import RIO.Text qualified as T
import Data.Text qualified as Text
import Prelude (read)

-- TODO This need to be reviewed and simplified moving foward.

newtype StateName = StateName Text deriving (Eq, Show)
instance IsString StateName where
    fromString = unsafeEr . fromText . T.pack
instance AsText StateName where
    fromText = return . StateName
    toText (StateName n) = n

newtype EventName = EventName Text deriving (Eq, Show)
instance AsText EventName where
    fromText = return . EventName
    toText (EventName n) = n
instance IsString EventName where
    fromString = unsafeEr . fromText . T.pack

newtype Version = Version (Int64, Int64)
    deriving (Generic)
    deriving newtype (Eq,Show)
instance FromJSON Version
instance ToJSON Version
instance Hashable Version
instance IsString Version where
    fromString = unsafeEr . fromText . T.pack
instance AsText Version where
    fromText t =
      let [a,b] = Text.splitOn "." t in Right (Version (read (T.unpack a), read (T.unpack b)))
    toText (Version (a, b)) = T.pack $ show a <> "." <> show b

newtype ChartName = ChartName Text
    deriving (Eq, Show, Ord, Generic)

-- | This is our canonical Chart representation.
data Chart s e = Chart
    { name :: Text
    , version :: Version
    , initial :: s
    , states :: [State s e]
    }
    deriving (Show, Eq, Generic) -- TODO are we using json here?

data Content e
    = Script Text -- TODO call it ActionName
    | Raise e
    deriving (Show, Eq, Generic, Functor)

data State s e
    = NormalState
        { sid :: s
        , transitions :: [Transition s e]
        , description :: Text
        , onEntry :: [Content e]
        , onExit :: [Content e]
        }
    | MultiState
        { sid :: s
        , msInitial :: s
        , subStates :: [State s e]
        , transitions :: [Transition s e]
        , description :: Text
        , onEntry :: [Content e]
        , onExit :: [Content e]
        }
    | Final
        { sid :: s
        , description :: Text
        , onEntry :: [Content e]
        , onExit :: [Content e]
        } -- not a list
    | Parallel
        { sid :: s
        , regions :: [State s e] -- regions should only contain MultiState or Parallel
        , transitions :: [Transition s e]
        , description :: Text
        , onEntry :: [Content e]
        , onExit :: [Content e]
        }
    deriving (Show, Eq, Generic)

-- | Also known as an "event". It connects one state to other in a specific direction;
data Transition s e = Transition
    { event' :: e
    , source :: s
    , target :: s
    }
    deriving (Show, Eq, Generic)

------------
-- HELPER --
------------

unsafeEr :: Either Text a -> a
unsafeEr = \case
    Left l -> error . T.unpack $ "UnsafeEr: " <> l
    Right r -> r

class AsText a where
    fromText :: Text -> Either Text a
    toText :: a -> Text

instance AsText Text where
    fromText = return
    toText = id


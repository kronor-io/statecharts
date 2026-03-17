module Statechart.Types where

import Data.Text qualified as Text
import Data.Text.Read qualified as Text.Read
import RIO
import RIO.Text qualified as T
import Prelude ((!!))

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

newtype Version = Version (Int64, Int64, Int64)
    deriving (Generic)
    deriving newtype (Eq, Show)
instance Hashable Version
instance IsString Version where
    fromString = unsafeEr . fromText . T.pack
instance AsText Version where
    fromText t =
        let fragments = Text.splitOn "." t
         in case length fragments of
                3 -> first Text.pack $ do
                    (a, aR) <- Text.Read.decimal (fragments !! 0)
                    (b, bR) <- Text.Read.decimal (fragments !! 1)
                    (c, cR) <- Text.Read.decimal (fragments !! 2)
                    if any (not . Text.null) [aR, bR, cR]
                        then Left "Failed to parse semver"
                        else pure $ Version (a, b, c)
                2 -> first Text.pack $ do
                    (a, aR) <- Text.Read.decimal (fragments !! 0)
                    (b, bR) <- Text.Read.decimal (fragments !! 1)
                    if any (not . Text.null) [aR, bR]
                        then Left "Failed to parse semver"
                        else pure $ Version (a, b, 0)
                1 -> first Text.pack $ do
                    (a, aR) <- Text.Read.decimal (fragments !! 0)
                    if not (Text.null aR)
                        then Left "Failed to parse semver"
                        else pure $ Version (a, 0, 0)
                _ -> Left "Failed to parse semver"
    toText (Version (a, b, c)) =
        if c == 0
            then T.pack $ show a <> "." <> show b
            else T.pack $ show a <> "." <> show b <> "." <> show c

newtype ChartName = ChartName Text
    deriving (Eq, Show, Ord, Generic)

-- | This is our canonical Chart representation.
data Chart s e = Chart
    { name :: Text
    , version :: Version
    , initial :: s
    , states :: [State s e]
    }
    deriving (Show, Eq, Generic)

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

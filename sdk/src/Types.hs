module Types where

import Data.Aeson
import Data.Text qualified as T
import RIO
import RIO.List (nub)

bimapState :: (s -> t) -> (e -> f) -> State s e -> State t f
bimapState f_ g_ = \case
    NormalState a b c d e -> NormalState (f_ a) (bimapEvent f_ g_ <$> b) c (fmap g_ <$> d) (fmap g_ <$> e)
    MultiState a b c d e f g ->
        MultiState
            (f_ a)
            (f_ b)
            (bimapState f_ g_ <$> c)
            (bimapEvent f_ g_ <$> d)
            e
            (fmap g_ <$> f)
            (fmap g_ <$> g)
    Final a b c d -> Final (f_ a) b (fmap g_ <$> c) (fmap g_ <$> d)
    Parallel{} -> error "not implemented"

bimapEvent :: (s -> t) -> (e -> f) -> Transition s e -> Transition t f
bimapEvent f_ g_ (Transition e s t) = Transition (g_ e) (f_ s) (f_ t)

getStateNames :: AsText s => Chart s e -> [StateName]
getStateNames = map (unsafeEr . fromText . toText . sid) . getAllChartStates

getEventNames :: AsText e => Chart s e -> [EventName]
getEventNames = nub . map (unsafeEr . fromText . toText . event') . getAllChartTransitions

getAllSubStates :: State s e -> [State s e]
getAllSubStates MultiState{..} = subStates ++ concatMap getAllSubStates subStates
getAllSubStates Parallel{..} = regions ++ concatMap getAllSubStates regions
getAllSubStates _ = []

getAllTransitions :: State s e -> [Transition s e]
getAllTransitions Final{} = []
getAllTransitions NormalState{..} = transitions
getAllTransitions MultiState{..} = transitions ++ concatMap getAllTransitions subStates
getAllTransitions Parallel{..} = transitions ++ concatMap getAllTransitions regions

getAllChartStates :: Chart s e -> [State s e]
getAllChartStates Chart{..} = states ++ concatMap getAllSubStates states

getAllChartTransitions :: Chart s e -> [Transition s e]
getAllChartTransitions Chart{..} = concatMap getAllTransitions states

isFinal :: State s e -> Bool
isFinal = \case Final{} -> True; _ -> False

isInitial :: Eq s => Chart s e -> State s e -> Bool
isInitial chart state = initial chart == id_ || any aux (states chart)
  where
    id_ = sid state
    aux Final{} = False
    aux NormalState{} = False
    aux MultiState{..} = msInitial == id_ || any aux subStates
    aux Parallel{..} = any aux regions

-- | Returns all the parent states of a given state with the most inmediate parent
-- as the first element all the wayt to the root.
-- Note: assumes the state only exists once inside the chart.
getParents :: Eq s => Chart s e -> State s e -> [State s e]
getParents chart state = case mapMaybe (aux []) (states chart) of
    [] -> error "State not found on chart"
    (x : _) -> x
  where
    id_ = sid state
    aux2 l acc = case mapMaybe (aux acc) l of
        [] -> Nothing
        (x : _) -> Just x
    aux acc Final{..} = if id_ == sid then Just acc else Nothing
    aux acc NormalState{..} = if id_ == sid then Just acc else Nothing
    aux acc s@MultiState{..} = if id_ == sid then Just acc else aux2 subStates (s : acc)
    aux acc s@Parallel{..} = if id_ == sid then Just acc else aux2 regions (s : acc)

unsafeEr :: Either Text a -> a
unsafeEr = \case
    Left l -> error . T.unpack $ "UnsafeEr: " <> l
    Right r -> r

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

newtype Version = Version (Int, Int)
    deriving (Generic)
    deriving newtype (Eq)
instance Show Version where
    show (Version (n, m)) = show n <> "." <> show m

instance FromJSON Version
instance ToJSON Version

instance Hashable Version

instance IsString Version where
    fromString = unsafeEr . fromText . T.pack

instance AsText Version where
    fromText _t = return $ Version (0, 1)
    toText (Version (a, b)) = T.pack $ show a <> show b

newtype ChartName = ChartName Text
    deriving (Eq, Show, Ord, Generic)
instance FromJSON ChartName
instance ToJSON ChartName

class AsText a where
    fromText :: Text -> Either Text a
    toText :: a -> Text

instance AsText Text where
    fromText = return
    toText = id

-- | This is our canonical Chart representation.
data Chart s e = Chart
    { version :: Version
    , initial :: s
    , states :: [State s e]
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance (Hashable s, Hashable e) => Hashable (Chart s e)

data Content e
    = Script Text -- TODO ActionName
    | Raise e
    deriving (Show, Eq, Generic, ToJSON, FromJSON, Functor)

instance (Hashable e) => Hashable (Content e)

data State s e
    = NormalState
        { sid :: s
        , transitions :: [Transition s e]
        , description :: Text
        , onEntry :: Maybe (Content e)
        , onExit :: Maybe (Content e)
        }
    | MultiState
        { sid :: s
        , msInitial :: s
        , subStates :: [State s e]
        , transitions :: [Transition s e]
        , description :: Text
        , onEntry :: Maybe (Content e)
        , onExit :: Maybe (Content e)
        }
    | Final
        { sid :: s
        , description :: Text
        , onEntry :: Maybe (Content e)
        , onExit :: Maybe (Content e)
        } -- not a list
    | Parallel
        { sid :: s
        , regions :: [State s e] -- regions should only contain MultiState or Parallel
        , transitions :: [Transition s e]
        , description :: Text
        , onEntry :: Maybe (Content e)
        , onExit :: Maybe (Content e)
        }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance (Hashable s, Hashable e) => Hashable (State s e)

data Transition s e = Transition
    { event' :: e
    , source :: s
    , target :: s
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance (Hashable s, Hashable e) => Hashable (Transition s e)

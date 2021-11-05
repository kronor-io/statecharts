module Statechart.Helpers where

import RIO
import RIO.List (nub,sort)
import Statechart.Types

-- TODO organize and delete the functions that are not been used anymore.

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

lookupState :: Chart StateName e -> StateName -> Maybe (State StateName e)
lookupState ch st =
  foldr go Nothing (getAllChartStates ch)
 where
  go _ (Just x) = Just x
  go a acc = if sid a == st then Just a else acc

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

getInitialState :: Chart StateName EventName -> State StateName EventName
getInitialState chart =
    case filter (isInitial chart) (getAllChartStates chart) of
        [ini] -> ini
        _ -> error "impossible"

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

getAllActions :: Ord e => Chart s e -> [Content e]
getAllActions Chart{..} =
  sort <$> actionFromStates =<< getAllChartStates Chart{..}
 where
  actionFromStates :: State s e -> [Content e]
  actionFromStates state = 
    case onEntry state of
      Nothing -> []
      Just xs -> [xs]

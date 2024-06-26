module Statechart.Helpers where

import RIO
import RIO.List (nub)
import Statechart.Types

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
    aux Parallel{regions} = any (\st -> sid st == id_) regions || any aux regions

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

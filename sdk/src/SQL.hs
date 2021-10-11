module SQL where

import Data.String.Interpolate (i, iii)
import Data.Text as T
import RIO
import Types

-- | Configuration for the generation.
data GenConfig = GenConfig
    { cfgName :: Text
    , cfgVersion :: Version
    }

-- | So we can generate SQL from any chart.
gen :: (AsText e, AsText s, Eq s) => GenConfig -> Chart s e -> Text
gen GenConfig{..} chart =
    let h = header cfgName
        s :: Text = stateArea chart
        t :: Text = transitionArea chart
        b = fnBody GenConfig{..} [iii|#{s}\n#{t}|]
     in [iii|#{h}#{b}\n|]

-------------
-- HELPERS --
-------------

-- | This is the area where we define the states inside the def.
stateArea :: (Eq s, AsText s, AsText e) => Chart s e -> Text
stateArea chart =
    let x = T.intercalate ",\n" (stateItemDef chart <$> getAllChartStates chart)
        header_ = ii "state"
     in [iii| #{header_} (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values\n#{x};|]

-- | This is the area where we define transitions inside the def.
transitionArea :: (AsText s, AsText e) => Chart s e -> Text
transitionArea chart =
    let x = T.intercalate ",\n" (transitionItemDef <$> getAllChartTransitions chart)
        header_ = ii "transition"
     in [iii| #{header_} (statechart_id, event, source_state, target_state) values\n#{x};|]

header :: Text -> Text
header name = [i|-- Deploy kronor:statechart/#{name} to pg\n\n-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN\n\n|]

fnBody :: GenConfig -> Text -> Text
fnBody GenConfig{..} body =
    [i|BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('#{cfgName}', #{cfgVersion}::semver) returning id into chart;
#{body}
end
$$;
COMMIT;|]

stateItemDef :: (AsText e, AsText s, Eq s) => Chart s e -> State s e -> Text
stateItemDef c s =
    tuple
        [ {-statechart_id-} "chart"
        , {-id-} str (toText (sid s))
        , {-name-} str (description s)
        , {-parent_id-} maybe nul (str . toText . sid) (head_ (getParents c s))
        , {-is_initial-} bul (isInitial c s)
        , {-is_final-} bul (isFinal s)
        , {-on_entry-} case cToText <$> onEntry s of
            Nothing -> "null"
            Just x -> if T.null x then "nully" else x
        , {-on_exit-} "null" -- TODO not implemented
        ]

cToText :: AsText e => Content e -> Text
cToText = \case
    Script t -> let (a, b) = T.breakOn "." t in tuple [str a, str $ T.drop 1 b]
    Raise e -> toText e

transitionItemDef :: (AsText s, AsText e) => Transition s e -> Text
transitionItemDef t =
    tuple
        [ "chart"
        , str (toText (event' t))
        , str (toText (source t))
        , str (toText (target t))
        ]

------------
-- SYNTAX --
------------

ii :: Text -> Text
ii n = [iii|insert into fsm.#{n}|]

-- | To create a tuple in the SQL format.
tuple :: [Text] -> Text
tuple [] = "()"
tuple xs = "(" <> T.intercalate ", " xs <> ")"

bul :: Bool -> Text
bul = \case True -> "true"; False -> "false"

-- | So we can quote the string the way sql does.
str :: Text -> Text
str a = [i|'#{a}'|]

nul :: Text
nul = "null"

head_ :: [a] -> Maybe a
head_ [] = Nothing
head_ [x] = Just x
head_ (x : _xs) = Just x
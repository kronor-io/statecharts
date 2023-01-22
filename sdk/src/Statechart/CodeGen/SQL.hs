-- | This module contains functions for going from charts to sql files.
-- Eventually it should be done with pretty printers to make it more robust (also pretty)
module Statechart.CodeGen.SQL (
    writeSQLs,
    generateSQL,
    generateSQLVerify,
    generateSQLRevert,
    GenConfig (..),
    gen,
) where

import Data.List qualified as List
import Data.String.Interpolate (i, iii)
import Data.Text as T
import RIO
import RIO.ByteString qualified as BS
import RIO.Text qualified as T
import Statechart.Helpers
import Statechart.Types
import System.FilePath.Posix (dropExtension)

writeSQLs :: FilePath -> [(FilePath, Text, Version, Text)] -> IO ()
writeSQLs targetPath xs =
    forM_ xs $ \(path, name, version, body) ->
        BS.writeFile (targetPath <> T.unpack (prepareName name) <> "-" <> T.unpack (toText version <> ".sql")) (T.encodeUtf8 body)
  where
    prepareName :: Text -> Text
    prepareName = T.replace "." "/"

mkCfgFile :: Text -> Version -> Text
mkCfgFile chartName chartVersion = prepareName chartName <> "-" <> toText chartVersion
  where
    prepareName :: Text -> Text
    prepareName = T.replace "." "/"

generateSQL :: Text -> [(FilePath, ByteString, Chart StateName EventName)] -> [(FilePath, Text, Version, Text)]
generateSQL prefix =
    fmap $ \(x, _bs, a) ->
        let code = gen (GenConfig prefix (mkCfgFile (name a) (version a)) (name a) (version a)) a
         in (x, name a, version a, code)

generateSQLVerify :: Text -> [(FilePath, ByteString, Chart StateName EventName)] -> [(FilePath, Text, Version, Text)]
generateSQLVerify prefix =
    fmap $ \(x, _bs, a) ->
        let code = genVerify (GenConfig prefix (mkCfgFile (name a) (version a)) (name a) (version a))
         in (x, name a, version a, code)

generateSQLRevert :: Text -> [(FilePath, ByteString, Chart StateName EventName)] -> [(FilePath, Text, Version, Text)]
generateSQLRevert prefix =
    fmap $ \(x, _bs, a) ->
        let code = genRevert (GenConfig prefix (mkCfgFile (name a) (version a)) (name a) (version a))
         in (x, name a, version a, code)

-------------
-- HELPERS --
-------------

-- | Configuration for the generation.
data GenConfig = GenConfig
    { cfgPrefix :: Text
    , cfgFile :: Text
    , cfgName :: Text
    , cfgVersion :: Version
    }

versionToSql :: Version  -> Text
versionToSql (Version (a, b, c)) =
  if c == 0
  then [i|#{a}.#{b}::semver|] -- compatibility with existing deploy scripts.
  else [i|'#{a}.#{b}.#{c}'::semver|]

-- | So we can generate a verify SQL script from any chart.
genVerify :: GenConfig -> Text
genVerify GenConfig{..} =
    let h = verifyHeader cfgPrefix cfgFile
     in [i|#{h}
BEGIN;

select *
from fsm.statechart
where name = '#{cfgName}'
and version = #{versionToSql cfgVersion}
limit 1;

ROLLBACK;
|]

-- | So we can generate a revert SQL script from any chart.
genRevert :: GenConfig -> Text
genRevert GenConfig{..} =
    let h = revertHeader cfgPrefix cfgFile
     in [i|#{h}
BEGIN;

with chart as (
    delete from fsm.statechart
    where name = '#{cfgName}'
    and version = #{versionToSql cfgVersion}
    returning id
)
delete from fsm.state
    where statechart_id = (select id from chart);

COMMIT;
|]

-- | So we can generate SQL from any chart.
gen :: (AsText e, AsText s, Eq s) => GenConfig -> Chart s e -> Text
gen GenConfig{..} chart =
    let h = header cfgPrefix cfgFile
        s :: Text = stateArea chart
        t :: Text = transitionArea chart
        b = fnBody GenConfig{..} [iii|#{s}\n#{t}|]
     in [iii|#{h}#{b}\n|]

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

verifyHeader :: Text -> Text -> Text
verifyHeader prefix name = [i|-- Verify #{prefix}/#{name} on pg\n\n-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN\n\n|]

revertHeader :: Text -> Text -> Text
revertHeader prefix name = [i|-- Revert #{prefix}/#{name} from pg\n\n-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN\n\n|]

header :: Text -> Text -> Text
header prefix name = [i|-- Deploy #{prefix}/#{name} to pg\n\n-- FILE AUTOMATICALLY GENERATED. MANUAL CHANGES MIGHT BE OVERWRITTEN\n\n|]

fnBody :: GenConfig -> Text -> Text
fnBody GenConfig{..} body =
    [i|BEGIN;
do $$
declare
chart bigint;
begin
insert into fsm.statechart (name, version) values ('#{cfgName}', #{versionToSql cfgVersion}) returning id into chart;
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
        , {-on_entry-} "array[" <> mconcat (List.intersperse "," . RIO.filter (not . T.null) . fmap cToText . onEntry $ s) <> "]::fsm_callback_name[]"
        , {-on_exit-} "array[" <> mconcat (List.intersperse "," . RIO.filter (not . T.null) . fmap cToText . onExit $ s) <> "]::fsm_callback_name[]"
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

-- can be removed
head_ :: [a] -> Maybe a
head_ [] = Nothing
head_ [x] = Just x
head_ (x : _xs) = Just x

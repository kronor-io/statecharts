-- | This module is used to generate Haskell code from a chart.
module Statechart.CodeGen.Haskell (writeHaskells, generateHaskell, genCodeFromChart, genCodeFromFile) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (foldl1)
import Language.Haskell.TH
import RIO
import RIO.ByteString qualified as BS
import RIO.Text qualified as T
import Statechart.Helpers
import Statechart.SCXML qualified as SCXML
import Statechart.Types as Types
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeExtension, dropExtension)
import Text.Casing
import Data.Char (toUpper)

import Prelude qualified
createDirectoryRecursive :: FilePath -> FilePath -> IO ()
createDirectoryRecursive src fp = do
    let dir = takeWhile (/= '/') fp
    if dir == "" || takeExtension dir /= ""
    then return ()
    else do
        Prelude.print [src,dir,fp]
        createDirectoryIfMissing True (src <> dir)
        createDirectoryRecursive (src <> dir <> "/") (drop 1 (dropWhile (/= '/') fp))

writeHaskells :: FilePath -> [(FilePath, Text)] -> IO ()
writeHaskells targetPath xs = do
    createDirectoryIfMissing True targetPath
    forM_ xs $ \(path, body) -> do
        let finalPath = targetPath <> dropExtension path <> ".hs"
        createDirectoryRecursive targetPath path
        BS.writeFile finalPath (T.encodeUtf8 body)

dotToDash :: String -> String
dotToDash [] = []
dotToDash ('.':c:xs) = '/':toUpper c: dotToDash xs
dotToDash (x:xs) = x:dotToDash xs

capsAfterDot :: String -> String
capsAfterDot [] = []
capsAfterDot ('.':c:xs) = '.':toUpper c : capsAfterDot xs
capsAfterDot (x:xs) = x : capsAfterDot xs

-- | This function needs to be in IO so we run the Q monad with the templates.
generateHaskell :: [(FilePath, ByteString, Chart StateName EventName)] -> IO [(FilePath, Text)]
generateHaskell =
    mapM $ \(_, __bs, a) -> do
        let mn = pascal (capsAfterDot (T.unpack $ name a))
            fn = (dotToDash mn) <> ".hs"
            flowName = filter (/= '.') mn
        code <- runQ $ genCodeFromChart (T.pack flowName) a
        let header = T.pack $ "module " <> mn <> " where\n\nimport RIO\nimport Types\n\n-- FILE AUTOMATICALLY\n-- GENERATED. DO NOT CHANGE IT\n-- MANUALLY. CHANGES MIGHT BE OVERWRITTEN.\n\n"
        return (fn, header <> T.pack (pprint code))

genCodeFromChart :: Text -> Chart StateName EventName -> Q [Dec]
genCodeFromChart flowName doc = do
    typesDec <- genTypesFromChart flowName doc
    variableDec <- genChartStructure flowName doc
    return $ typesDec <> variableDec

parseAndGen :: Text -> ByteString -> Q [Dec]
parseAndGen flowName raw = do
    let doc_ = SCXML.parse (LBS.fromStrict raw)
    case doc_ of
        Left e -> error $ "GEN CODE: " <> show e
        Right doc -> genCodeFromChart flowName doc

genCodeFromFile :: Text -> Text -> Q [Dec]
genCodeFromFile flowName (T.unpack -> path) = do
    x <- runIO (LBS.readFile path)
    parseAndGen flowName (LBS.toStrict x)

-- | Generates the datatype and instances for a flows state,
-- state names must be in snake_case
genStateTypes :: FlowName -> [Text] -> Q [Dec]
genStateTypes flowName stateNames =
    return
        [ DataD [] dataName [] Nothing (map (`NormalC` []) names) [DerivClause Nothing [nShow, nEq, nOrd]]
        , InstanceD
            Nothing
            []
            (AppT (nameC "AsText") (ConT dataName))
            [FunD (mkName "toText") (zipWith (\name real -> Clause [ConP name []] (NormalB (LitE (StringL real))) []) names (T.unpack <$> stateNames))]
        ]
  where
    dataName = mkName . pascal . T.unpack $ flowName <> "_states"
    names = map (stateTypeName flowName) stateNames

-- | Generates the datatype and instances for a flows state,
-- state names must be in snake_case
genEventTypes :: FlowName -> [Text] -> Q [Dec]
genEventTypes flowName eventNames =
    return
        [ DataD [] dataName [] Nothing (map (`NormalC` []) names) [DerivClause Nothing [nShow, nEq, nOrd]]
        , InstanceD
            Nothing
            []
            (AppT (nameC "AsText") (ConT dataName))
            [FunD (mkName "toText") (zipWith (\name real -> Clause [ConP name []] (NormalB (LitE (StringL real))) []) names (T.unpack <$> eventNames))]
        ]
  where
    dataName = mkName . pascal . T.unpack $ flowName <> "_events"
    names = map (eventTypeName flowName) eventNames

stateValueName :: Text -> Language.Haskell.TH.Name
stateValueName state = mkName $ "state" <> pascal (T.unpack state)

eventTypeName :: FlowName -> Text -> Language.Haskell.TH.Name
eventTypeName flowName = makeEventTypeName . T.unpack
  where
    eventDataNameText = flowName <> "_events"
    replacePointsForUnderScores = map (\c -> if c == '.' then '_' else c)
    makeEventTypeName = mkName . (pascal (T.unpack eventDataNameText) <>) . pascal . replacePointsForUnderScores

stateTypeName :: FlowName -> Text -> Language.Haskell.TH.Name
stateTypeName flowName = stateToName . T.unpack
  where
    stateDataNameText = flowName <> "_states"
    stateToName = mkName . (pascal (T.unpack stateDataNameText) <>) . pascal

----------------------------------------------------------------------------

genChartStructure :: (AsText e, AsText s) => FlowName -> Chart s e -> Q [Dec]
genChartStructure flowName Chart{..} = do
    let variable = ValD (VarP variableName) (NormalB bodyExp) $ map (genStateDec flowName) (getAllChartStates Chart{..})
        bodyExp =
            applyExpression
                [ nameE "Chart"
                , LitE (StringL $ T.unpack $ toText version)
                , ConE initialStateTypeName
                , ListE $ map (VarE . stateValueName . toText . sid) states
                ]
    return [variable]
  where
    variableName = mkName $ camel (T.unpack flowName)
    dataNameText = flowName <> "_states"
    initialStateTypeName = mkName . (pascal (T.unpack dataNameText) <>) . pascal $ T.unpack (toText initial)

genTypesFromChart :: (AsText e, AsText s) => FlowName -> Chart s e -> Q [Dec]
genTypesFromChart flowName chart = do
    stateCode <- genStateTypes flowName stateNames
    eventsCode <- genEventTypes flowName eventNames
    return $ stateCode <> eventsCode
  where
    stateNames = map toText (getStateNames chart)
    eventNames = map toText (getEventNames chart)

genTransitionExp :: (AsText e, AsText s) => FlowName -> Text -> Transition s e -> Exp
genTransitionExp flowName parentStateName Transition{..} =
    applyExpression
        [ nameE "Transition"
        , ConE $ eventTypeName flowName (toText event')
        , ConE $ stateTypeName flowName parentStateName
        , ConE $ stateTypeName flowName (toText target)
        ]

genContentExp :: AsText e => FlowName -> Content e -> Exp
genContentExp _ (Script src) = applyExpression [nameE "Script", LitE . StringL $ T.unpack src]
genContentExp flowName (Raise event) = applyExpression [nameE "Raise", ConE $ eventTypeName flowName (toText event)]

genStateDec :: (AsText e, AsText s) => FlowName -> State s e -> Dec
genStateDec flowName state =
    ValD
        (VarP decName)
        (NormalB $ bodyExp state)
        []
  where
    dataNameText = flowName <> "_states"
    stateTypeName_ = mkName . (pascal (T.unpack dataNameText) <>) . pascal $ T.unpack $ toText $ sid state
    decName = stateValueName $ toText $ sid state
    onentryExp = ListE . fmap (genContentExp flowName) . onEntry $ state
    onexitExp = ListE . fmap (genContentExp flowName) . onExit $ state
    bodyExp :: (AsText s, AsText e) => State s e -> Exp
    bodyExp Final{..} =
        applyExpression
            [ nameE "Final"
            , ConE stateTypeName_
            , LitE . StringL . T.unpack $ description
            , onentryExp
            , onexitExp
            ]
    bodyExp NormalState{..} =
        applyExpression
            [ nameE "NormalState"
            , ConE stateTypeName_
            , ListE $ map (genTransitionExp flowName (toText sid)) transitions
            , LitE . StringL . T.unpack $ description
            , onentryExp
            , onexitExp
            ]
    bodyExp Parallel{..} =
        applyExpression
            [ nameE "Parallel"
            , ConE stateTypeName_
            , ListE $ VarE . stateValueName . toText . Types.sid <$> regions
            , ListE $ genTransitionExp flowName (toText sid) <$> transitions
            , LitE . StringL $ T.unpack description
            , onentryExp
            , onexitExp
            ]
    bodyExp MultiState{..} =
        applyExpression
            [ nameE "MultiState"
            , ConE stateTypeName_
            , nameE . (pascal (T.unpack dataNameText) <>) . pascal . T.unpack . toText $ msInitial
            , ListE $ VarE . stateValueName . toText . Types.sid <$> subStates -- sid
            , ListE $ genTransitionExp flowName (toText sid) <$> transitions
            , LitE . StringL . T.unpack $ description
            , onentryExp
            , onexitExp
            ]

-------------
-- HELPERS --
-------------

nameC :: String -> Type
nameC = ConT . mkName

nameE :: String -> Exp
nameE = ConE . mkName

nShow :: Type
nShow = nameC "Show"

nEq :: Type
nEq = nameC "Eq"

nOrd :: Type
nOrd = nameC "Ord"

type FlowName = Text

applyExpression :: [Exp] -> Exp
applyExpression = foldl1 AppE

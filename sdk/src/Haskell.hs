{-# LANGUAGE TemplateHaskell #-}

module Haskell where

import Data.ByteString.Lazy qualified as LBS
import Data.List (foldl1)
import Language.Haskell.TH
import RIO
import RIO.Text qualified as T
import SCXML
import Text.Casing
import Types

genCode' :: Text -> Chart StateName EventName -> Q [Dec]
genCode' flowName doc = do
    typesDec <- genTypesFromChart flowName doc
    variableDec <- genChartStructure flowName doc
    return $ typesDec <> variableDec

genCode :: Text -> ByteString -> Q [Dec]
genCode flowName x = do
    let doc_ = SCXML.parse (LBS.fromStrict x)
    case doc_ of
        Left e -> error $ "GEN CODE: " <> show e
        Right doc -> genCode' flowName doc

genCodeFromFile :: Text -> Text -> Q [Dec]
genCodeFromFile flowName (T.unpack -> path) = do
    x <- runIO (LBS.readFile path)
    genCode flowName (LBS.toStrict x)

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
genContentExp _ (Script src) = applyExpression [nameE "Just", applyExpression [nameE "Script", LitE . StringL $ T.unpack src]]
genContentExp flowName (Raise event) = applyExpression [nameE "Just", applyExpression [nameE "Raise", ConE $ eventTypeName flowName (toText event)]]

orNothing :: Maybe Exp -> Exp
orNothing = \case
    Nothing -> nameE "Nothing"
    Just e -> e

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
    onentryExp = orNothing $ genContentExp flowName <$> onEntry state
    onexitExp = orNothing $ genContentExp flowName <$> onExit state
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

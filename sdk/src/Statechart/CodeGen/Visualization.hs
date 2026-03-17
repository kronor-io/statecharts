module Statechart.CodeGen.Visualization where

import Data.Text qualified as T
import RIO
import Statechart.Types

generatePlantuml :: (AsText s, AsText e, Show s, Show e) => Chart s e -> Text
generatePlantuml c =
    T.unlines $
        concat
            [
                [ "@startuml"
                , ""
                , "hide empty description"
                , ""
                , T.unwords ["note", T.pack (show (name c <> " version " <> toText (version c))), "as Name"]
                , ""
                , T.unwords ["[*] -->", toText (initial c)]
                ]
            , concatMap generateStateUml (states c)
            ,
                [ ""
                , "@enduml"
                ]
            ]

generateStateUml :: (AsText s, AsText e, Show s, Show e) => State s e -> [Text]
generateStateUml s = case s of
    NormalState{..} ->
        concat
            [
                [ generateStateId sid
                , T.unwords [generateStateDescription sid description]
                ]
            , map generateTransitionUml transitions
            , map (generateOnEntry sid) onEntry
            , map (generateOnExit sid) onExit
            ]
    MultiState{..} ->
        concat
            [
                [ T.unwords [generateStateId sid, "{"]
                ]
            , map
                ("  " <>)
                $ concat
                    [
                        [ T.unwords [generateStateDescription sid description]
                        , T.unwords ["[*] -->", toText msInitial]
                        ]
                    , concatMap generateStateUml subStates
                    , map (generateOnEntry sid) onEntry
                    , map (generateOnExit sid) onExit
                    ]
            , ["}"]
            ]
            ++ map generateTransitionUml transitions
    Final{..} ->
        concat
            [
                [ generateStateId sid
                , T.unwords [generateStateDescription sid description]
                , T.unwords [toText sid, "--> [*]"]
                ]
            , map (generateOnEntry sid) onEntry
            , map (generateOnExit sid) onExit
            ]
    Parallel{..} ->
        concat
            [
                [ T.unwords [generateStateId sid, "{"]
                ]
            , map
                ("  " <>)
                $ concat
                    [ [T.unwords [generateStateDescription sid description]]
                    , concatMap (\ss -> generateStateUml ss ++ ["||"]) regions
                    , map (generateOnEntry sid) onEntry
                    , map (generateOnExit sid) onExit
                    ]
            , ["}"]
            ]
            ++ map generateTransitionUml transitions

generateStateId :: (AsText s) => s -> Text
generateStateId sid =
    T.unwords ["state", toText sid]

generateStateDescription :: (AsText s) => s -> Text -> Text
generateStateDescription sid description =
    T.unwords [toText sid, ":", description]

generateTransitionUml :: (AsText s, AsText e) => Transition s e -> Text
generateTransitionUml Transition{..} =
    T.unwords
        [ toText source
        , if "done.state." `T.isPrefixOf` toText event'
            then "-[#green,bold]->"
            else "-->"
        , toText target
        , ":"
        , toText event'
        ]

generateOnEntry :: (AsText s, AsText e) => s -> Content e -> Text
generateOnEntry sid = generateContent sid "onentry"

generateOnExit :: (AsText s, AsText e) => s -> Content e -> Text
generateOnExit sid = generateContent sid "onexit"

generateContent :: (AsText s, AsText e) => s -> Text -> Content e -> Text
generateContent s d = \case
    Script t -> T.unwords [toText s, ":", d, t]
    Raise e -> T.unwords [toText s, ":", d, (toText e)]

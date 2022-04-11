-- | This is the parser for the SCXML file format.
module Statechart.SCXML (readSCXMLfiles, parse, parseRoot) where

import Data.Map.Strict qualified as Map
import RIO
import RIO.ByteString qualified as BS
import RIO.ByteString.Lazy qualified as LBS
import RIO.Text qualified as T
import Statechart.Types
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath.Posix (takeExtension)
import Text.XML
import Text.XML qualified as XML
import Text.XML.Cursor
import Prelude qualified

-- TODO isnt this related to any IO? seems more like a helper
-- XXX will consume lot of memory if used on large directories
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive src = go src
    where
        go d = do
            contents <- listDirectory d
            innerDirs <- filterM (ddeS src) contents
            res <- go1 innerDirs
            pure $ contents ++ res

        go1 [] = pure []
        go1 (d:ds) = do
            contents <- (\cs -> (\c -> d <> "/" <> c) <$> cs) <$> listDirectory (src <> d)
            innerDirs <- filterM (ddeS (src <> "/")) contents
            res <- go1 (innerDirs++ds)
            pure $ contents ++ res
        ddeS src c = doesDirectoryExist (src <> c)

readSCXMLfiles :: FilePath -> IO [(FilePath, ByteString, Chart StateName EventName)]
readSCXMLfiles sourcePath = do
    xs_ <- filter scxmlFile <$> listDirectoryRecursive sourcePath
    forM (zip xs_ xs_) $ \(path, _) -> do
        a <- BS.readFile (sourcePath <> path)
        case parse $ LBS.fromStrict a of
            Left e -> error . show $ e
            Right p -> return (path, a, p)
 where
   scxmlFile a = takeExtension a == ".scxml"

-- | We use this to go from XML to our canonical Chart type.
parse :: LBS.ByteString -> Either Text (Chart StateName EventName)
parse = parseRoot . fromDocument . parseLBS_ def

parseRoot :: Cursor -> Either Text (Chart StateName EventName)
parseRoot cursor = do
    n <- fromText =<< getElemAttr "name" cursor
    v <- fromText =<< getElemAttr "version" cursor
    i <- fromText =<< getElemAttr "initial" cursor
    s <- getSubStates cursor
    return $ Chart n v i s

getInitial :: Cursor -> Either Text StateName
getInitial cursor = case childs of
    [] -> Left $ "No initial element in: " <> T.pack (show cursor)
    [c1] -> case transitions c1 of
        [] -> Left "Initial has no transition"
        [c2] -> fromText =<< getElemAttr "target" c2
        l -> Left $ "Initial must only contain one transition, found: " <> T.pack (show (length l))
    _ -> Left $ "More than one inital element found in: " <> T.pack (show cursor)
  where
    childs = getChildsWithName ["initial"] cursor
    transitions c = getChildsWithName ["transition"] c

getElemAttr :: XML.Name -> Cursor -> Either Text Text
getElemAttr name c = case node c of
    NodeElement e ->
        case Map.lookup name . elementAttributes $ e of
            Nothing -> Left ("Attribute \"" <> T.pack (show name) <> "\" not found in: " <> T.pack (show c))
            Just xx -> Right xx
    _ -> Left $ "Bad usage of getElemAttr, trying to lookup \"" <> T.pack (show name) <> "\" in: " <> T.pack (show c)

getChildsWithName :: [Text] -> Cursor -> [Cursor]
getChildsWithName types = filter isOfType . child
  where
    isOfType c = case node c of
        NodeElement e -> nameLocalName (elementName e) `elem` types
        _x -> False

getNodeName :: Cursor -> Either Text Text
getNodeName cursor = case node cursor of
    NodeElement e -> Right $ nameLocalName (elementName e)
    _ -> Left $ "Bad usage of getNodeName when trying to get name of: " <> T.pack (show cursor)

maybeGetElemAttr :: XML.Name -> Cursor -> Either Text Text
maybeGetElemAttr name c = case node c of
    NodeElement e -> case Map.lookup name . elementAttributes $ e of
        Nothing -> Left "not found..."
        Just xx -> Right xx
    _ -> Left $ "Bad usage of maybeGetElemAttr, trying to lookup \"" <> T.pack (show name) <> "\" in: " <> T.pack (show c)

getContent :: Cursor -> Either Text [Content EventName]
getContent cursor = mapM toContent childs
  where
    childs = getChildsWithName ["raise", "script"] cursor
    toContent c =
        contentType >>= \case
            "raise" -> do
                ev <- fromText =<< getElemAttr "event" c
                Right (Raise ev)
            "script" -> Script <$> getElemAttr "src" c
            _ -> Left "This should not happen, at getContent."
      where
        contentType = getNodeName c

data TriggerType = OnEntry | OnExit
                 deriving (Eq)

getContentOfTriggerType :: TriggerType -> Cursor -> Either Text [Content EventName]
getContentOfTriggerType tt = fmap join . traverse getContent . getChildsWithName [triggerName tt]
    where
        triggerName OnEntry = "onentry"
        triggerName OnExit = "onexit"

getTransitions :: StateName -> Cursor -> Either Text [Transition StateName EventName]
getTransitions src cursor = mapM toTransition childs
  where
    childs = getChildsWithName ["transition"] cursor
    toTransition c = do
        ev <- fromText =<< getElemAttr "event" c
        tg <- fromText =<< getElemAttr "target" c
        Right $ Transition{event' = ev, source = src, target = tg}

getSubStates :: Cursor -> Either Text [State StateName EventName]
getSubStates cursor = mapM toState childs
  where
    childs = getChildsWithName ["state", "parallel", "final"] cursor
    toState c = do
        sid <- fromText =<< getElemAttr "id" c
        subStates_ <- getSubStates c
        case stateType_ of
            Right "final" -> do
                Right $
                    Final
                        { sid = sid
                        , description = fromRight "" name_
                        , onEntry = entry_
                        , onExit = exit_
                        }
            Right "parallel" -> do
                ts <- transitions_ sid
                Right $
                    Parallel
                        { sid = sid
                        , regions = subStates_
                        , transitions = ts
                        , description = fromRight "" name_
                        , onEntry = entry_
                        , onExit = exit_
                        }
            Right "state" -> do
                ts <- transitions_ sid
                if null subStates_
                    then
                        return $
                            NormalState
                                { sid = sid
                                , transitions = ts
                                , description = fromRight "" name_
                                , onEntry = entry_
                                , onExit = exit_
                                }
                    else do
                        initial_ <- getInitial c
                        return $
                            MultiState
                                { sid = sid
                                , msInitial = initial_
                                , subStates = subStates_
                                , transitions = ts
                                , description = fromRight "" name_
                                , onEntry = entry_
                                , onExit = exit_
                                }
            _ -> Left "This also should not happen, at getSubStates."
      where
        entry_ = fromRight [] $ getContentOfTriggerType OnEntry c
        exit_ = fromRight [] $ getContentOfTriggerType OnExit c
        name_ = maybeGetElemAttr "name" c
        transitions_ src = getTransitions src c
        stateType_ = getNodeName c

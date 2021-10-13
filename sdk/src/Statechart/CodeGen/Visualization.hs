module Statechart.CodeGen.Visualization where

import Codec.Archive.Tar qualified as Tar
import Data.String.Interpolate (i)
import Data.Text as T
import RIO
import RIO.ByteString qualified as BS
import RIO.Text qualified as T
import Statechart.Types
import System.Directory (doesDirectoryExist)

-- | We use this function to bring the javascript visualization tool from the statecharts repo to here.
prepareVisuFiles :: FilePath -> IO ()
prepareVisuFiles targetPath = do
    let tarPath = "statecharts/data/visu.tar"
    visuExists <- doesDirectoryExist (targetPath <> "/visualization")
    unless visuExists $ do
        Tar.extract targetPath tarPath
        return ()

writeVisu :: FilePath -> Text -> IO ()
writeVisu targetPath = BS.writeFile targetPath . T.encodeUtf8

-- | This is used to prepare the javascript visualization tool to show the charts to us.
generateVisualizationData :: [(FilePath, ByteString, Chart StateName EventName)] -> Text
generateVisualizationData =
    (header <>) . RIO.foldr genItems "\n];"
  where
    header = "var scxml = ["
    item name dat = [i|\n{name: '#{name}', data: `#{dat}`}|]
    genItems (f, bs, _) acc = item f bs <> (if acc == "\n];" then acc else "," <> acc)

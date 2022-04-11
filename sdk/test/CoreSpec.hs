module CoreSpec (spec) where

import Helper
import Plugin.Haskell.Chart1
import Plugin.Haskell.Chart2
import Plugin.Haskell.Chart3
import RIO.ByteString qualified as BS
import RIO.Map qualified as M
import RIO.Text qualified as T
import Statechart.CodeGen.SQL qualified as SQL
import System.Directory

spec :: Spec
spec = do
    runSpec "chart1" "0.1" chart1
    runSpec "chart2" "2.0" chart2
    runSpec "chart3" "0.3" chart3

runSpec :: (AsText a, AsText b, Eq a) => Text -> Version -> Chart a b -> Spec
runSpec name semver chart = do
    expectedSql <- fromRight undefined . T.decodeUtf8' <$> runIO (BS.readFile $ "test/Plugin/SQL/" <> T.unpack name <> ".sql")
    describe (T.unpack name <> " sql generation") $ do
        it "returns what we expect" (gensSql name semver chart expectedSql)

gensSql :: (Eq s, AsText e, AsText s) => Text -> Version -> Chart s e -> Text -> Expectation
gensSql name semver chart result =
  SQL.gen (SQL.GenConfig name name semver) chart `shouldBe` result

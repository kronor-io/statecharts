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
    runSpec "chart1" chart1
    runSpec "chart2" chart2
    runSpec "chart3" chart3

runSpec :: (AsText a, AsText b, Eq a) => Text -> Chart a b -> Spec
runSpec name y = do
    foo <- fromRight undefined . T.decodeUtf8' <$> runIO (BS.readFile $ "test/Plugin/SQL/" <> T.unpack name <> ".sql")
    describe (T.unpack name <> " sql generation") $ it "returns what we expect" (gensSql name y foo)

gensSql :: (Eq s, AsText e, AsText s) => Text -> Chart s e -> Text -> Expectation
gensSql name chart result = SQL.gen (SQL.GenConfig name "0.1") chart `shouldBe` result

gensSqlNot :: (Eq s, AsText e, AsText s) => Text -> Chart s e -> Text -> Expectation
gensSqlNot name chart result = SQL.gen (SQL.GenConfig name "0.1") chart `shouldNotBe` result

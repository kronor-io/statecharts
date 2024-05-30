module CoreSpec (spec) where

import Helper
import Plugin.Haskell.Chart1
import Plugin.Haskell.Chart2
import Plugin.Haskell.Chart3
import RIO.Text qualified as T
import Statechart.CodeGen.SQL qualified as SQL

spec :: Spec
spec = do
    runSpec "chart1" "1.1" chart1
    runSpec "chart2" "2.0" chart2
    runSpec "chart3" "0.3" chart3

runSpec :: (AsText a, AsText b, Eq a) => Text -> Version -> Chart a b -> Spec
runSpec name semver chart = do
    describe (T.unpack name <> " sql generation") $ do
        it "returns the same sql as before" $
            pureGoldenTextFile ("test/Plugin/SQL/" <> T.unpack name <> ".sql") $
                SQL.gen (SQL.GenConfig "kronor:statechart" name name semver) chart

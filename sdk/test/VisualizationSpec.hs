module VisualizationSpec (spec) where

import Helper
import RIO.ByteString.Lazy qualified as LB
import Statechart.SCXML as SCXML
import Statechart.CodeGen.Visualization

spec :: Spec
spec = do
    runSpec "chart1" 
    runSpec "chart2" 
    runSpec "chart3" 

runSpec :: String ->  Spec
runSpec name = do
    let inputFile = "test/Plugin/SCXML/" <> name <> ".scxml"
    let expectedFile = "test/Plugin/Visualisation/" <> name <> ".pml"
    describe (name <> " plantuml generation") $ do
        it "returns what we expect" $ do
            goldenTextFile expectedFile $ do
                inputScxml <- fromRight undefined . SCXML.parse <$> LB.readFile inputFile
                pure $  generatePlantuml inputScxml


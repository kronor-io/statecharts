{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module GenerateChart (main) where

import Options.Applicative
import Path
import Path.IO hiding (findExecutable)
import RIO
import RIO.ByteString qualified as BS
import RIO.ByteString.Lazy qualified as LBS
import RIO.Text (pack)
import Statechart.CodeGen.SQL
import Statechart.CodeGen.Visualization (generatePlantuml)
import Statechart.SCXML as SCXML
import System.Directory (findExecutable)
import System.Environment (getArgs)
import System.FilePath qualified as F ((</>))
import System.Process.Typed (proc, runProcess_)

data Options = Options
    { project :: String
    , databaseDir :: String
    , files :: [String]
    }
    deriving (Show)

optionsParser :: Parser Options
optionsParser =
    Options
        <$> strOption
            ( long "project"
                <> metavar "PROJECT"
                <> help "Sqitch project that should contain the statecharts"
            )
        <*> strOption
            ( long "database-dir"
                <> metavar "DATABASE_DIR"
                <> showDefault
                <> value "database"
                <> help "Sqitch project that should contain the statecharts"
            )
        <*> option
            auto
            ( long "files"
                <> help "Generate only these statecharts"
                <> value []
                <> metavar "FILES"
            )

opts :: ParserInfo Options
opts =
    info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Generate sqitch based sql migrations from scxml files"
            <> header "generate-chart"
        )

main :: IO ()
main = do
    options <- execParser opts

    chartDir <- resolveDir' (options.databaseDir F.</> "deploy/statechart")
    deployDir <- resolveDir' (options.databaseDir F.</> "deploy/statechart")
    verifyDir <- resolveDir' (options.databaseDir F.</> "verify/statechart")
    revertDir <- resolveDir' (options.databaseDir F.</> "revert/statechart")

    scxmls <- case options.files of
        [] -> map (first (chartDir </>)) <$> readSCXMLfiles chartDir
        _ -> for options.files $ \fp -> do
            af <- resolveFile' fp
            a <- BS.readFile (fromAbsFile af)
            case parse $ LBS.fromStrict a of
                Left e -> error . show $ e
                Right p -> return (af, p)

    let charts = map snd scxmls

    -- Generation
    let sqls = generateSQL (pack $ options.project <> ":statechart") charts
        verifySqls = generateSQLVerify (pack $ options.project <> ":statechart") charts
        revertSqls = generateSQLRevert (pack $ options.project <> ":statechart") charts

    -- Writing to disk
    writeSQLs deployDir sqls
    writeSQLs verifyDir verifySqls
    writeSQLs revertDir revertSqls

    needThen "plantuml" "Skipping generation of svg as plantuml is not found in PATH" do
        for_ scxmls $ \(path, chart) -> do
            umlPath <- addExtension ".pml" path
            let umlCode = generatePlantuml chart
            ensureDir $ parent umlPath
            writeFileUtf8 (fromAbsFile umlPath) umlCode
            runProcess_ $ proc "plantuml" ["-tsvg", fromAbsFile umlPath]

need :: String -> IO ()
need exe = do
    mSqitchExe <- findExecutable exe
    when (isNothing mSqitchExe) $ do
        putStrLn ("Aborting program as executable " <> exe <> " not found")
        exitFailure

needThen :: String -> String -> IO a -> IO ()
needThen exe notExistsMsg whenExists = do
    mSqitchExe <- findExecutable exe
    case mSqitchExe of
        Just _ -> do
            void whenExists
        Nothing -> do
            putStrLn notExistsMsg

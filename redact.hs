------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : redact: hide secret text on the terminal
-- Copyright   : Copyright (c) 2020-2021 Travis Cardwell
-- License     : MIT
--
-- See the README for details.
------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- https://hackage.haskell.org/package/ansi-terminal
import qualified System.Console.ANSI as Term

-- https://hackage.haskell.org/package/ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import Control.Applicative ((<|>), optional)
import Control.Monad (foldM, forM_, unless)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Char (isSpace, toLower)
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.Version (showVersion)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO
  ( Handle, IOMode(ReadMode), hIsEOF, hGetLine, hPutStrLn, stderr, stdin
  , withFile
  )

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- (redact:cabal)
import qualified Paths_redact as Project

-- (redact:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Constants

defaultColor :: Term.Color
defaultColor = Term.Red

defaultIntensity :: Term.ColorIntensity
defaultIntensity = Term.Vivid

envColorName, envIntensityName :: String
envColorName     = "REDACT_COLOR"
envIntensityName = "REDACT_INTENSITY"

configFileName :: FilePath
configFileName = "redact.ini"

------------------------------------------------------------------------------
-- $Colors

colorMap :: [(String, Term.Color)]
colorMap =
    [ (toLower <$> show color, color)
    | color <- [minBound ..]
    ]

parseColor
  :: String
  -> Either String Term.Color
parseColor s
    = maybe (Left $ "uknown color: " ++ s) Right
    $ lookup (toLower <$> s) colorMap

------------------------------------------------------------------------------
-- $Intensities

intensityMap :: [(String, Term.ColorIntensity)]
intensityMap =
    [ (toLower <$> show intensity, intensity)
    | intensity <- [minBound ..]
    ]

parseIntensity
  :: String
  -> Either String Term.ColorIntensity
parseIntensity s
    = maybe (Left $ "unknown intensity: " ++ s) Right
    $ lookup (toLower <$> s) intensityMap

------------------------------------------------------------------------------
-- $Options

-- | Program options
data Options f
  = Options
    { optColor     :: !(f Term.Color)
    , optIntensity :: !(f Term.ColorIntensity)
    , optFile      :: !(Maybe FilePath)
    , optColors    :: !Bool
    , optTest      :: !Bool
    }

getOptions :: IO (Options Identity)
getOptions = do
    opts0 <- parseArgs

    opts1 <- if isJust (optColor opts0)
      then return opts0
      else do
        mColor <- getEnvColor
        return opts0 { optColor = mColor }
    opts2 <- if isJust (optIntensity opts1)
      then return opts1
      else do
        mIntensity <- getEnvIntensity
        return opts1 { optIntensity = mIntensity }

    case (optColor opts2, optIntensity opts2) of
      (Just color, Just intensity) -> return Options
        { optColor     = pure color
        , optIntensity = pure intensity
        , optFile      = optFile opts2
        , optColors    = optColors opts2
        , optTest      = optTest opts2
        }
      _otherOptions -> do
        configOpts <- loadConfigFile
        return Options
          { optColor = pure . fromMaybe defaultColor $
              optColor opts2 <|> configColor configOpts
          , optIntensity = pure . fromMaybe defaultIntensity $
              optIntensity opts2 <|> configIntensity configOpts
          , optFile   = optFile opts2
          , optColors = optColors opts2
          , optTest   = optTest opts2
          }

------------------------------------------------------------------------------
-- $Arguments

parseArgs :: IO (Options Maybe)
parseArgs = OA.execParser pinfo
  where
    pinfo :: OA.ParserInfo (Options Maybe)
    pinfo
      = OA.info (LibOA.helper <*> LibOA.versioner version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "hide secret text on the terminal"
          , OA.failureCode 2
          , OA.footerDoc $ Just footer
          ]

    version :: String
    version = "redact-haskell " ++ showVersion Project.version

    options :: OA.Parser (Options Maybe)
    options =
      Options
        <$> colorOption
        <*> intensityOption
        <*> fileOption
        <*> colorsOption
        <*> testOption

    footer :: Doc
    footer = colorsHelp LibOA.<||> intensitiesHelp LibOA.<||> settingsHelp

    colorsHelp :: Doc
    colorsHelp = LibOA.section "Colors:" . Doc.text $
      intercalate ", " (fst <$> colorMap)

    intensitiesHelp :: Doc
    intensitiesHelp = LibOA.section "Intensities:" . Doc.text $
      intercalate ", " (fst <$> intensityMap)

    settingsHelp :: Doc
    settingsHelp = LibOA.section "Settings priority:" . Doc.vcat $
      [ Doc.text "1. command-line options (--color, --intensity)"
      , Doc.text $
          "2. environment variables (" ++
          envColorName ++ ", " ++ envIntensityName ++ ")"
      , Doc.text ("3. settings file (" ++ configFileName ++ ")") Doc.<$$>
          ( Doc.indent 5 . Doc.vcat $
              [ Doc.text "color=COLOR"
              , Doc.text "intensity=INTENSITY"
              ]
          )
      , Doc.text "4. defaults" Doc.<$$>
          ( Doc.indent 5 . LibOA.table_ 2 $
              [ ["color:", toLower <$> show defaultColor]
              , ["intensity:", toLower <$> show defaultIntensity]
              ]
          )
      ]

colorOption :: OA.Parser (Maybe Term.Color)
colorOption = optional . OA.option (OA.eitherReader parseColor) $ mconcat
    [ OA.long "color"
    , OA.short 'c'
    , OA.metavar "COLOR"
    , OA.help "redacted text color"
    ]

intensityOption :: OA.Parser (Maybe Term.ColorIntensity)
intensityOption = optional . OA.option (OA.eitherReader parseIntensity) $
    mconcat
      [ OA.long "intensity"
      , OA.short 'i'
      , OA.metavar "INTENSITY"
      , OA.help "redacted text color intensity"
      ]

fileOption :: OA.Parser (Maybe FilePath)
fileOption = optional . OA.strOption $ mconcat
    [ OA.long "file"
    , OA.short 'f'
    , OA.metavar "PATH"
    , OA.help "input file (default: STDIN)"
    ]

colorsOption :: OA.Parser Bool
colorsOption = OA.switch $ mconcat
    [ OA.long "colors"
    , OA.help "redact test text using all possible colors"
    ]

testOption :: OA.Parser Bool
testOption = OA.switch $ mconcat
    [ OA.long "test"
    , OA.help "redact test text using the configured color"
    ]

------------------------------------------------------------------------------
-- $EnvVars

getEnv
  :: String
  -> (String -> Either String a)
  -> IO (Maybe a)
getEnv envName parse = do
    meex <- fmap parse <$> lookupEnv envName
    case meex of
      Nothing -> return Nothing
      Just (Right x) -> return $ Just x
      Just (Left err) -> errorExit $
        "environment variable " ++ envName ++ ": " ++ err

getEnvColor :: IO (Maybe Term.Color)
getEnvColor = getEnv envColorName parseColor

getEnvIntensity :: IO (Maybe Term.ColorIntensity)
getEnvIntensity = getEnv envIntensityName parseIntensity

------------------------------------------------------------------------------
-- $ConfigFile

data ConfigFileOptions
  = ConfigFileOptions
    { configColor     :: !(Maybe Term.Color)
    , configIntensity :: !(Maybe Term.ColorIntensity)
    }

loadConfigFile :: IO ConfigFileOptions
loadConfigFile = do
    path <- Dir.getXdgDirectory Dir.XdgConfig configFileName
    exists <- Dir.doesFileExist path
    if exists
      then foldM go defOpts . zip [1..] . map strip . lines =<< readFile path
      else return defOpts
  where
    defOpts :: ConfigFileOptions
    defOpts = ConfigFileOptions
      { configColor     = Nothing
      , configIntensity = Nothing
      }

    go :: ConfigFileOptions -> (Int, String) -> IO ConfigFileOptions
    go opts (_, "") = return opts
    go opts (_, '#' : _) = return opts
    go opts (lineNum, line) = case bimap strip strip (break (== '=') line) of
      ("", _) -> errorExit $ "config file: empty key on line " ++ show lineNum
      (key, '=' : value) -> case key of
        "color" -> case parseColor value of
          Right color -> return opts { configColor = Just color }
          Left err -> errorExit $
            "config file: color error on line " ++ show lineNum ++ ": " ++ err
        "intensity" -> case parseIntensity value of
          Right intensity -> return opts { configIntensity = Just intensity }
          Left err -> errorExit $
            "config file: intensity error on line " ++ show lineNum ++ ": " ++
            err
        _otherKey -> return opts
      _mistmatch ->
        errorExit $ "config file: unable to parse line " ++ show lineNum

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
{-# INLINE strip #-}

------------------------------------------------------------------------------
-- $Implementation

redactLine :: Term.Color -> Term.ColorIntensity -> String -> IO ()
redactLine color intensity = go
  where
    go :: String -> IO ()
    go "" = putStrLn ""
    go ('`' : s) = do
      putStr "`"
      let (hiddenText, rest) = break (== '`') s
      unless (null hiddenText) $ do
        Term.setSGR
          [ Term.SetColor Term.Foreground intensity color
          , Term.SetColor Term.Background intensity color
          ]
        putStr hiddenText
        Term.setSGR [Term.Reset]
      case rest of
        '`' : s' -> putStr "`" >> go s'
        _emptyString -> putStrLn ""
    go s = do
      let (clearText, rest) = break (== '`') s
      putStr clearText
      go rest

runHandle :: Term.Color -> Term.ColorIntensity -> Handle -> IO ()
runHandle color intensity handle = go =<< hIsEOF handle
  where
    go :: Bool -> IO ()
    go True  = return ()
    go False = do
      s <- hGetLine handle
      if getFenceLength s >= 3
        then do
          putStrLn s
          Term.setSGR
            [ Term.SetColor Term.Foreground intensity color
            , Term.SetColor Term.Background intensity color
            ]
          goFence s =<< hIsEOF handle
        else do
          redactLine color intensity s
          go =<< hIsEOF handle

    getFenceLength :: String -> Int
    getFenceLength s
      | all (== '`') s = length s
      | otherwise      = 0

    goFence :: String -> Bool -> IO ()
    goFence _     True  = Term.setSGR [Term.Reset]
    goFence fence False = do
      s <- hGetLine handle
      if s == fence
        then do
          Term.setSGR [Term.Reset]
          putStrLn s
          go =<< hIsEOF handle
        else do
          putStrLn s
          goFence fence =<< hIsEOF handle

runFile :: Term.Color -> Term.ColorIntensity -> FilePath -> IO ()
runFile color intensity path =
    withFile path ReadMode $ runHandle color intensity

runTest :: Term.Color -> Term.ColorIntensity -> IO ()
runTest color intensity = redactLine color intensity $
    (toLower <$> show color) ++ " " ++ (toLower <$> show intensity) ++
    ": `hidden`"

runColors :: IO ()
runColors =
    forM_ [minBound ..] $ \color ->
      forM_ [minBound ..] $ \intensity ->
        runTest color intensity

------------------------------------------------------------------------------
-- $CLI

errorExit :: String -> IO a
errorExit message = do
    hPutStrLn stderr $ "error: " ++ message
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    Options{..} <- getOptions
    let color     = runIdentity optColor
        intensity = runIdentity optIntensity
    case (optColors, optTest, optFile) of
      (True, _,    _)         -> runColors
      (_,    True, _)         -> runTest color intensity
      (_,    _,    Nothing)   -> runHandle color intensity stdin
      (_,    _,    Just path) -> runFile color intensity path

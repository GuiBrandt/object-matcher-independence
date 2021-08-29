{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (optional)
import Control.Monad (forM_, (<=<))
import Data.Attoparsec.ByteString (many1', parseOnly)
import qualified Data.ByteString.UTF8 as BSU
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.List (intercalate)
import Data.ObjectMatcher (ObjectMatcher (..))
import Internal.Utils (pairs)
import Main.Report (reportOn)
import Options.Applicative
import Parser (objectMatcher)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (IOMode (..), hGetContents, isEOF, openFile)

data Options = Options
  { rulesFile :: Maybe String,
    inputFile :: Maybe String
  }

main :: IO ()
main = do
  options <- execParser $ info options mempty
  rules <- maybe (return []) readFromFile (rulesFile options)
  matchers <- maybe readStdin readFromFile (inputFile options)
  reportOn rules matchers

options :: Parser Options
options =
  let rulesOpt = optional $ strOption (short 'r' <> long "rules" <> metavar "FILENAME")
      fileOpt = optional $ strOption (short 'f' <> long "file" <> metavar "FILENAME")
   in Options <$> rulesOpt <*> fileOpt

readFromFile :: String -> IO [ObjectMatcher]
readFromFile = readMatchers <=< readFile

readStdin :: IO [ObjectMatcher]
readStdin = stdin >>= readMatchers

stdin :: IO String
stdin = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      (line ++) <$> stdin

readMatchers :: String -> IO [ObjectMatcher]
readMatchers str =
  case parseOnly (many1' objectMatcher) (BSU.fromString str) of
    Left err -> do
      putStrLn err
      exitWith $ ExitFailure 3
    Right matchers -> return matchers

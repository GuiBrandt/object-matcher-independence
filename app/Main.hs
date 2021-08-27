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
import Internal.Utils (pairs)
import Lib
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
  matchers <- maybe readInput readFromFile (inputFile options)
  reportOn rules matchers
  where
    options :: Parser Options
    options =
      let rulesOpt = optional $ strOption (short 'r' <> long "rules" <> metavar "FILENAME")
          fileOpt = optional $ strOption (short 'f' <> long "file" <> metavar "FILENAME")
       in Options <$> rulesOpt <*> fileOpt

    readFromFile :: String -> IO [ObjectMatcher]
    readFromFile = readMatchers <=< readFile

    readFile :: String -> IO String
    readFile file = do
      handle <- openFile file ReadMode
      hGetContents handle

    readInput :: IO [ObjectMatcher]
    readInput = stdin >>= readMatchers

    readMatchers :: String -> IO [ObjectMatcher]
    readMatchers str =
      case parseOnly (many1' objectMatcher) (BSU.fromString str) of
        Left err -> do
          putStrLn err
          exitWith $ ExitFailure 3
        Right matchers -> return matchers

    stdin :: IO String
    stdin = do
      done <- isEOF
      if done
        then return []
        else do
          line <- getLine
          (line ++) <$> stdin

    reportOn :: [ObjectMatcher] -> [ObjectMatcher] -> IO ()
    reportOn rules ms = do
      reportOnRules rules
      reportOnMatchers rules ms

    reportOnRules :: [ObjectMatcher] -> IO ()
    reportOnRules rules = do
      if null rules
        then return ()
        else do
          if satisfiable $ Conjunction rules
            then do
              putStrLn "Using rules:"
              forM_ rules print
              putStrLn ""
            else do
              putStrLn "Rules are unsatisfiable"
              exitWith $ ExitFailure 4

    reportOnMatchers :: [ObjectMatcher] -> [ObjectMatcher] -> IO ()
    reportOnMatchers rules ms = do
      let unsatisfiable = findUnsatisfiable rules ms
      case unsatisfiable of
        [] -> return ()
        _ -> do
          putStrLn "Found unsatisfiable matchers:"
          forM_ unsatisfiable print
          putStrLn ""

      let overlapping = findOverlapping rules ms
          independent = maximalIndependentSets ms overlapping
      case overlapping of
        [] -> do
          putStrLn "All matchers are independent"
          exitSuccess
        _ -> do
          putStrLn "Found overlapping matchers:"
          forM_ overlapping printOverlap
          putStrLn ""
          putStrLn "Maximal independent sets:"
          forM_ independent (\ms' -> putStrLn $ " - " ++ intercalate "\n   " (show <$> ms'))
          exitWith $ ExitFailure 1

    printOverlap :: Overlap -> IO ()
    printOverlap (Overlap (p, q) examples) =
      do
        putStrLn ""
        print p
        print q
        putStrLn ""
        putStrLn "Overlapping instances (at most 10 shown):"
        forM_ (take 10 examples) (\ex -> putStrLn $ " - " ++ intercalate ", " (show <$> ex))

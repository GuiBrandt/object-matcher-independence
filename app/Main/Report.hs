module Main.Report where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (forM_)
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.ObjectMatcher (ObjectMatcher (..))
import Data.ObjectMatcher.Independence (Result (..), check, maximalIndependentSets')
import Data.ObjectMatcher.Satisfiability (Solution, satisfiable)
import Internal.Utils (pairs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)

data Overlap = Overlap
  { matchers :: (ObjectMatcher, ObjectMatcher),
    examples :: [Solution]
  }

reportOn :: [ObjectMatcher] -> [ObjectMatcher] -> IO ()
reportOn rules ms = do
  reportOnRules rules
  reportOnMatchers (mconcat rules) ms

reportOnRules :: [ObjectMatcher] -> IO ()
reportOnRules rules
  | null rules = return ()
  | satisfiable $ mconcat rules =
    do
      putStrLn "Using rules:"
      forM_ rules print
      putStrLn ""
  | otherwise =
    do
      putStrLn "Rules are unsatisfiable"
      exitWith $ ExitFailure 4

reportOnMatchers :: ObjectMatcher -> [ObjectMatcher] -> IO ()
reportOnMatchers rules ms = do
  case filter (not . withRules satisfiable) ms of
    [] -> return ()
    unsatisfiable -> do
      putStrLn "Found unsatisfiable matchers:"
      forM_ unsatisfiable print
      putStrLn ""
  case [Overlap pq exs | (pq, CounterExamples exs) <- results] of
    [] -> do
      putStrLn "All matchers are independent"
      exitSuccess
    overlapping -> do
      putStrLn "Found overlapping matchers:"
      forM_ overlapping printOverlap
      putStrLn ""
      putStrLn "Maximal independent sets:"
      let independent = maximalIndependentSets' memoizedCheck ms
      forM_ independent (\ms' -> putStrLn $ " - " ++ intercalate "\n   " (show <$> ms'))
      exitWith $ ExitFailure 1
  where
    withRules = (. (rules <>))
    results = [(pq, withRules check p q) | pq@(p, q) <- pairs ms]
    memo = Map.fromList results
    memoizedCheck p q = fromMaybe (check p q) $ Map.lookup (p, q) memo <|> Map.lookup (q, p) memo

printOverlap :: Overlap -> IO ()
printOverlap (Overlap (p, q) examples) =
  do
    putStrLn ""
    print p
    print q
    putStrLn ""
    putStrLn "Overlapping instances (at most 10 shown):"
    forM_ (take 10 examples) (\ex -> putStrLn $ " - " ++ intercalate ", " (show <$> ex))

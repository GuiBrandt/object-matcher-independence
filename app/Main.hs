{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser (objectMatcher)

import Lib (intersection, ObjectMatcher(..), AttributeAssignment)
import Internal.Utils (pairs)

import Control.Monad (forM_)

import Data.List (intercalate)
import qualified Data.ByteString.UTF8 as BSU
import Data.Attoparsec.ByteString (parseOnly, many1')
import Data.Algorithm.MaximalCliques (getMaximalCliques)
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

import System.IO (isEOF)
import System.Exit (exitWith, ExitCode(..), exitSuccess)
import Data.Foldable (foldl')

readAll :: IO String
readAll = do done <- isEOF
             if done then return []
             else do line <- getLine
                     (line ++) <$> readAll

data Overlap = Overlap {
    matchers    :: (ObjectMatcher, ObjectMatcher)
    , examples  :: [[AttributeAssignment]]
}

findOverlapping :: [ObjectMatcher] -> [Overlap]
findOverlapping ms = [Overlap (p, q) int | (p, q) <- pairs ms, let int = intersection p q, int /= []]

maximalIndependentSets :: [ObjectMatcher] -> [Overlap] -> [[ObjectMatcher]]
maximalIndependentSets ms overlapping =
    let incompatible = do (p, q) <- matchers <$> overlapping; [(p, q), (q, p)]
        edges = curry $ not . (`elem` incompatible)
    in getMaximalCliques edges ms

report :: [ObjectMatcher] -> IO ()
report ms =
    let overlapping = findOverlapping ms
        independent = maximalIndependentSets ms overlapping
    in case overlapping of
        [] -> do putStrLn "All matchers are independent"
                 exitSuccess
        _  -> do putStrLn "Found overlapping matchers:"
                 forM_ overlapping printOverlap
                 putStrLn ""
                 putStrLn "Maximal independent sets:"
                 forM_ independent (\ms' -> putStrLn $ " - " ++ intercalate "\n   " (show <$> ms'))
                 exitWith (ExitFailure 1)

printOverlap :: Overlap -> IO ()
printOverlap (Overlap (p, q) examples) =
    do putStrLn ""
       print p
       print q
       putStrLn ""
       putStrLn "Overlapping instances (at most 10 shown):"
       forM_ (take 10 examples) (\ex -> putStrLn $ " - " ++ intercalate ", " (show <$> ex))

main :: IO ()
main = do
        input <- readAll
        case parseOnly (many1' objectMatcher) (BSU.fromString input) of
            Left err       -> do putStrLn err
                                 exitWith (ExitFailure 3)
            Right matchers -> report matchers

{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (isEOF)

import Lib (intersection, ObjectMatcher(..), AttributeAssignment)
import Utils (pairs)

import Control.Monad ((<=<), forM_)

import Data.List (intercalate)
import Data.Aeson (json', Value(Array, Object, String))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Attoparsec.ByteString (parseOnly, many1')
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as Map

readAll :: IO String
readAll = do done <- isEOF
             if done then return []
             else do line <- getLine
                     (line ++) <$> readAll

parseObjectMatchers :: String -> Either String [ObjectMatcher]
parseObjectMatchers = allOrNone . fmap valueToObjectMatcher <=< parseOnly (many1' json') . BSU.fromString
    where
        allOrNone :: [Either a b] -> Either a [b]
        allOrNone []             = Right []
        allOrNone ((Left l):_)   = Left l
        allOrNone ((Right r):xs) = (r:) <$> allOrNone xs

        stringValue :: Value -> Either String String
        stringValue (String text) = Right $ T.unpack text
        stringValue _             = Left "Value must be a string"

        valueToObjectMatcher :: Value -> Either String ObjectMatcher
        valueToObjectMatcher (Array arr) = Conjunction <$> allOrNone (map valueToObjectMatcher $ V.toList arr)
        valueToObjectMatcher (Object obj)
            | Map.member "not"       obj = Not <$> valueToObjectMatcher (obj ! "not")
            | Map.member "attribute" obj =
                case obj ! "attribute" of
                    String attribute ->
                        if Map.member "values" obj
                        then case obj ! "values" of
                            Array values -> Attribute (T.unpack attribute) <$> allOrNone (map stringValue $ V.toList values)
                            _ -> Left "Attribute values must be an array"
                        else Left "Missing values on attribute matcher"
                    _ -> Left "Attribute name must be a string"
        valueToObjectMatcher _ = Left "Failed to read: not a valid object matcher"

checkIndependence :: ObjectMatcher -> ObjectMatcher -> IO ()
checkIndependence a b =
    case intersection a b of
        []       -> return ()
        examples -> do putStrLn "Found overlapping matchers:"
                       putStrLn $ " - " ++ show a
                       putStrLn $ " - " ++ show b
                       putStrLn "Examples:"
                       forM_ (take 10 examples) printExample
                       putStrLn ""
    where
        printExample :: [AttributeAssignment] -> IO ()
        printExample example = putStrLn $ " - " ++ intercalate ", " (map show example)

main :: IO ()
main = do
        input <- readAll
        case parseObjectMatchers input of
            Left err    -> putStrLn err
            Right matchers -> mapM_ (uncurry checkIndependence) $ pairs matchers

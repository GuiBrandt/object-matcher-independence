{-# LANGUAGE OverloadedStrings #-}

module Parser (objectMatcher) where

import Lib (ObjectMatcher(..))

import Data.Aeson (json', Value(Array, Object, String))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Attoparsec.ByteString (parseOnly, many1', Parser)
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as Map

import Control.Monad ((<=<))
import Control.Monad.Fail (fail)

objectMatcher :: Parser ObjectMatcher
objectMatcher = do value <- json'
                   case toObjectMatcher value of
                     Left err -> fail err
                     Right om -> return om

toObjectMatcher :: Value -> Either String ObjectMatcher
toObjectMatcher (Array arr) = conjunction $ V.toList arr
toObjectMatcher (Object obj)
    | Map.member "not"       obj = negation $ obj ! "not"
    | Map.member "attribute" obj =
        let name = obj ! "attribute"
            values = Map.lookup "values" obj
        in maybe (Left "Missing values on attribute matcher") (attribute name) values
toObjectMatcher _ = Left "Failed to read: not a valid object matcher"

conjunction :: [Value] -> Either String ObjectMatcher
conjunction = fmap Conjunction . allOrNone . fmap toObjectMatcher

negation :: Value -> Either String ObjectMatcher
negation = fmap Not . toObjectMatcher

attribute :: Value -> Value -> Either String ObjectMatcher
attribute (String name) (Array values) = Attribute (T.unpack name) <$> allOrNone (map stringValue $ V.toList values)
attribute _ _ = Left "Attribute name must be a string, and values must be an array of strings"

stringValue :: Value -> Either String String
stringValue (String text) = Right $ T.unpack text
stringValue _             = Left "Value must be a string"

allOrNone :: [Either a b] -> Either a [b]
allOrNone []             = Right []
allOrNone ((Left l):_)   = Left l
allOrNone ((Right r):xs) = (r:) <$> allOrNone xs

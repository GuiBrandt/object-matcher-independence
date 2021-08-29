{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Data.ObjectMatcher
-- Copyright   :  2021 Guilherme G. Brandt
-- License     :  MIT
-- Maintainer  :  gui.g.brandt@gmail.com
-- Stability   :  experimental
-- Portability :  portable
module Data.ObjectMatcher where

import Data.Hashable (Hashable)
import Data.List (intercalate)
import GHC.Generics (Generic)

-- | The type of object matchers.
--
-- An object matcher is a predicate on the values of JSON-like objects.
--
-- More precisely, it is one of:
--
-- * An /attribute matcher/ of the form \( x \in S \), for some attribute \(x\)
--   and set of strings \(S\);
-- * The /conjunction/ of object matchers;
-- * The /negation/ of an object matcher.
data ObjectMatcher
  = Attribute String [String]
  | Conjunction [ObjectMatcher]
  | Not ObjectMatcher
  deriving (Eq, Generic)

instance Hashable ObjectMatcher

instance Show ObjectMatcher where
  show (Attribute name [value]) = name ++ " = \"" ++ value ++ "\""
  show (Attribute name values) = name ++ " ∈ {" ++ intercalate ", " (map (\x -> "\"" ++ x ++ "\"") values) ++ "}"
  show (Conjunction []) = "∅"
  show (Conjunction [matcher]) = show matcher
  show (Conjunction matchers) = intercalate " ∧ " $ show <$> matchers
  show (Not matcher) = "¬(" ++ show matcher ++ ")"

-- | '(<>)' corresponds to conjunction on 'ObjectMatcher's.
instance Semigroup ObjectMatcher where
  (Conjunction []) <> m = m
  m <> (Conjunction []) = m
  (Conjunction ps) <> (Conjunction qs) = Conjunction $ ps ++ qs
  (Conjunction ms) <> m = Conjunction $ m : ms
  m <> (Conjunction ms) = Conjunction $ m : ms
  p <> q = Conjunction [p, q]

-- | 'mempty' is @ObjectMatcher []@
instance Monoid ObjectMatcher where
  mempty = Conjunction []

-- |
-- Module      :  Data.ObjectMatcher.Internal
-- Copyright   :  2021 Guilherme G. Brandt
-- License     :  MIT
-- Maintainer  :  gui.g.brandt@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- = Introduction
--
-- This module exposes functions used internally to convert from
-- 'Data.ObjectMatcher.ObjectMatcher's to the representation accepted by
-- [minisat-solver](https://hackage.haskell.org/package/minisat-solver).
module Data.ObjectMatcher.Internal (Formula, toFormula, fundeps) where

import Control.Arrow (Arrow ((&&&)))
import Data.AttributeAssignment
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.ObjectMatcher
import Internal.Utils (pairs)
import qualified SAT.MiniSat as SAT

-- | Type for logical formulas on attribute assignments.
type Formula = SAT.Formula AttributeAssignment

-- | Converts an 'Data.ObjectMatcher.ObjectMatcher' into an equivalent 'Formula'.
--
-- >>> let p = Conjunction [Attribute "x" ["1", "2"], Not $ Attribute "y" ["3"]]
-- >>> toFormula p
-- All [Var x = "1" :->: Not (Var x = "2")] :&&: All [Some [Var x = "1",Var x = "2"],Not (Var y = "3")]
toFormula :: ObjectMatcher -> Formula
toFormula = uncurry (SAT.:&&:) . (fundeps &&& toFormula')
  where
    toFormula' :: ObjectMatcher -> Formula
    toFormula' (Attribute name [value]) = SAT.Var $ name :==: value
    toFormula' (Attribute name values) = SAT.Some $ SAT.Var . (name :==:) <$> values
    toFormula' (Conjunction [matcher]) = toFormula' matcher
    toFormula' (Conjunction matchers) = SAT.All $ toFormula' <$> matchers
    toFormula' (Not matcher) = SAT.Not $ toFormula' matcher

-- | /Functional dependencies/ are implications between argument matchers,
-- which correspond to the mutual exclusivity condition on equalities for the
-- attribute matchers on the object matcher.
-- This function maps an object matcher to a formula corresponding to a
-- minimal complete set of functional dependencies.
--
-- >>> let p = Conjunction [Attribute "x" ["1", "2"], Attribute "x" ["3"]]
-- >>> fundeps p
-- All [Var x = "1" :->: Not (Var x = "2"),Var x = "1" :->: Not (Var x = "3"),Var x = "2" :->: Not (Var x = "3")]
fundeps :: ObjectMatcher -> Formula
fundeps = SAT.All . concatMap (uncurry fundeps') . Map.assocs . mergeAttributes
  where
    fundeps' :: String -> HashSet String -> [Formula]
    fundeps' name values =
      let assignments = SAT.Var . (name :==:) <$> Set.toList values
       in [p SAT.:->: SAT.Not q | (p, q) <- pairs assignments]

    mergeAttributes :: ObjectMatcher -> Map String (HashSet String)
    mergeAttributes (Attribute name values) = Map.singleton name (Set.fromList values)
    mergeAttributes (Conjunction matchers) = foldl (Map.unionWith Set.union) Map.empty $ mergeAttributes <$> matchers
    mergeAttributes (Not matcher) = mergeAttributes matcher

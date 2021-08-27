module Lib
  ( ObjectMatcher (..),
    AttributeAssignment (..),
    Overlap (..),
    intersection,
    satisfiable,
    findUnsatisfiable,
    findOverlapping,
    maximalIndependentSets,
  )
where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Data.Algorithm.MaximalCliques (getMaximalCliques)
import Data.AttributeAssignment
import Data.Bifunctor (second)
import Data.List (find, groupBy, intercalate, tails)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.ObjectMatcher
import Data.Set (Set)
import qualified Data.Set as Set
import Internal.Utils
import SAT.MiniSat (Formula ((:&&:), (:->:)))
import qualified SAT.MiniSat as SAT

data Overlap = Overlap
  { matchers :: (ObjectMatcher, ObjectMatcher),
    examples :: [[AttributeAssignment]]
  }

satisfiable :: ObjectMatcher -> Bool
satisfiable matcher = SAT.satisfiable $ toFormula matcher

findUnsatisfiable :: [ObjectMatcher] -> [ObjectMatcher] -> [ObjectMatcher]
findUnsatisfiable rules = filter (null . intersection rules mempty)

findOverlapping :: [ObjectMatcher] -> [ObjectMatcher] -> [Overlap]
findOverlapping rules ms = [Overlap (p, q) int | (p, q) <- pairs ms, let int = intersection' p q, int /= []]
  where
    intersection' = intersection rules

maximalIndependentSets :: [ObjectMatcher] -> [Overlap] -> [[ObjectMatcher]]
maximalIndependentSets ms overlapping =
  let incompatible = do (p, q) <- matchers <$> overlapping; [(p, q), (q, p)]
      edges = curry $ not . (`elem` incompatible)
   in getMaximalCliques edges ms

-- >>> let p = Not $ Conjunction [Attribute "x" ["1"], Attribute "z" ["4", "5"]]
-- >>> let q = Conjunction [Attribute "y" ["2"], Not $ Attribute "x" ["1", "3"]]
-- >>> case intersection p q of [] -> Nothing; xs -> Just $ head xs
-- Just [x != "1",x != "3",y == "2",z == "4"]

intersection :: [ObjectMatcher] -> ObjectMatcher -> ObjectMatcher -> [[AttributeAssignment]]
intersection rules p q =
  let formula = toFormula $ mconcat rules <> p <> q
      solutions = SAT.solve_all formula
      assignments = fmap (uncurry truth) . Map.assocs <$> solutions
   in simplify <$> assignments

-- >>> simplify ["x" :==: "1", "x" :!=: "2", "x" :!=: "3", "y" :!=: "4", "y" :!=: "5"]
-- [x == "1",y != "4",y != "5"]

simplify :: [AttributeAssignment] -> [AttributeAssignment]
simplify = isolatePositive <=< groupByAttribute
  where
    isolatePositive :: [AttributeAssignment] -> [AttributeAssignment]
    isolatePositive assignments = maybe assignments (: []) $ find positive assignments

    groupByAttribute :: [AttributeAssignment] -> [[AttributeAssignment]]
    groupByAttribute = groupBy (\p q -> attributeName p == attributeName q)

-- >>> let p = Not $ Conjunction [Attribute "x" ["1"], Attribute "z" ["4", "5"]]
-- >>> toFormula p
-- All [Var z == "4" :->: Not (Var z == "5")] :&&: Not (All [Var x == "1",Some [Var z == "4",Var z == "5"]])

toFormula :: ObjectMatcher -> SAT.Formula AttributeAssignment
toFormula = uncurry (:&&:) . (fundeps &&& toFormula')
  where
    toFormula' :: ObjectMatcher -> SAT.Formula AttributeAssignment
    toFormula' (Attribute name [value]) = SAT.Var $ name :==: value
    toFormula' (Attribute name values) = SAT.Some $ SAT.Var . (name :==:) <$> values
    toFormula' (Conjunction [matcher]) = toFormula' matcher
    toFormula' (Conjunction matchers) = SAT.All $ toFormula' <$> matchers
    toFormula' (Not matcher) = SAT.Not $ toFormula' matcher

-- >>> let p = Conjunction [Attribute "x" ["1", "2"], Attribute "y" ["3", "4"]]
-- >>> fundeps p
-- All [Var x == "1" :->: Not (Var x == "2"),Var y == "3" :->: Not (Var y == "4")]

fundeps :: ObjectMatcher -> SAT.Formula AttributeAssignment
fundeps = SAT.All . concatMap (uncurry fundeps' . second Set.elems) . Map.assocs . mergeAttributes
  where
    fundeps' :: String -> [String] -> [Formula AttributeAssignment]
    fundeps' name values =
      let assignments = SAT.Var . (name :==:) <$> values
       in [p :->: SAT.Not q | (p, q) <- pairs assignments]

    mergeAttributes :: ObjectMatcher -> Map String (Set String)
    mergeAttributes (Attribute name values) = Map.singleton name (Set.fromList values)
    mergeAttributes (Conjunction matchers) = foldl (Map.unionWith Set.union) Map.empty $ mergeAttributes <$> matchers
    mergeAttributes (Not matcher) = mergeAttributes matcher

module Lib ( ObjectMatcher(..), AttributeAssignment(..), intersection ) where

import Data.ObjectMatcher
import Data.AttributeAssignment
import Utils

import Data.List (tails, intercalate, find, groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)

import Control.Monad ((<=<))
import Control.Arrow ((&&&))

import qualified SAT.MiniSat as SAT
import SAT.MiniSat (Formula((:&&:), (:->:)))

-- >>> let p = Not $ Conjunction [Attribute "x" ["1"], Attribute "z" ["4", "5"]]
-- >>> let q = Conjunction [Attribute "y" ["2"], Not $ Attribute "x" ["1", "3"]]
-- >>> case intersection p q of [] -> Nothing; xs -> Just $ head xs
-- Just [x != "1",x != "3",y == "2",z == "4"]

intersection :: ObjectMatcher -> ObjectMatcher -> [[AttributeAssignment]]
intersection p q =
    let formula     = toFormula $ p <> q
        solutions   = SAT.solve_all formula
        assignments = fmap (uncurry truth) . Map.assocs <$> solutions
    in simplify <$> assignments

-- >>> simplify ["x" :==: "1", "x" :!=: "2", "x" :!=: "3", "y" :!=: "4", "y" :!=: "5"]
-- [x == "1",y != "4",y != "5"]

simplify :: [AttributeAssignment] -> [AttributeAssignment]
simplify = isolatePositive <=< groupByAttribute
    where
        isolatePositive :: [AttributeAssignment] -> [AttributeAssignment]
        isolatePositive assignments = maybe assignments (:[]) $ find positive assignments
        
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
        toFormula' (Attribute name values)  = SAT.Some $ SAT.Var . (name :==:) <$> values
        toFormula' (Conjunction [matcher])  = toFormula' matcher
        toFormula' (Conjunction matchers)   = SAT.All $ toFormula' <$> matchers
        toFormula' (Not matcher)            = SAT.Not $ toFormula' matcher

-- >>> let p = Conjunction [Attribute "x" ["1", "2"], Attribute "y" ["3", "4"]]
-- >>> fundeps p
-- All [Var x == "1" :->: Not (Var x == "2"),Var y == "3" :->: Not (Var y == "4")]

fundeps :: ObjectMatcher -> SAT.Formula AttributeAssignment
fundeps = SAT.All . concatMap (uncurry fundeps' . second Set.elems) . Map.assocs . mergeAttributes
    where
        fundeps' :: String -> [String] -> [Formula AttributeAssignment]
        fundeps' name values = let assignments = SAT.Var . (name :==:) <$> values
                               in [ p :->: SAT.Not q | (p, q) <- pairs assignments ]
        
        mergeAttributes :: ObjectMatcher -> Map String (Set String)
        mergeAttributes (Attribute name values) = Map.singleton name (Set.fromList values)
        mergeAttributes (Conjunction matchers)  = foldl (Map.unionWith Set.union) Map.empty $ mergeAttributes <$> matchers
        mergeAttributes (Not matcher)           = mergeAttributes matcher

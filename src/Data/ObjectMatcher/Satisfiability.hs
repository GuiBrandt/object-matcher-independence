-- |
-- Module      :  Data.ObjectMatcher.Satisfiability
-- Copyright   :  2021 Guilherme G. Brandt
-- License     :  MIT
-- Maintainer  :  gui.g.brandt@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- = Introduction
--
-- An 'Data.ObjectMatcher.ObjectMatcher' is /satisfiable/ if there exists an
-- object that is matched by it.
--
-- This module provide functions for checking whether an object matcher is
-- satisfiable as well as finding a set of attribute assignments that
-- satisfies it.
--
-- This is done by reducing those problems to [SAT](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem)
-- and solving them with [minisat-solver](https://hackage.haskell.org/package/minisat-solver).
module Data.ObjectMatcher.Satisfiability (Solution, solve, satisfiable) where

import Control.Monad ((<=<))
import Data.AttributeAssignment (AttributeAssignment (..), attributeName, negated, positive)
import Data.List (find, groupBy)
import qualified Data.Map.Strict as Map
import Data.ObjectMatcher (ObjectMatcher (..))
import Data.ObjectMatcher.Internal (toFormula)
import qualified SAT.MiniSat as SAT

-- | Determines whether an object matcher is satisfiable.
--
-- >>> let p = Conjunction [Attribute "x" ["1"], Attribute "x" ["2"]]
-- >>> satisfiable p
-- False
satisfiable :: ObjectMatcher -> Bool
satisfiable = SAT.satisfiable . toFormula

-- | The type for sets of attribute assignments that satisfy some object
-- matcher
type Solution = [AttributeAssignment]

-- | Finds all 'Solution's that satisfy a given object matcher.
--
-- >>> let p = Conjunction [Attribute "x" ["1", "2"], Attribute "y" ["3"]]
-- >>> solve p
-- [[x = "2",y = "3"],[x = "1",y = "3"]]
solve :: ObjectMatcher -> [Solution]
solve = fmap (simplify . convertSolution) . SAT.solve_all . toFormula
  where
    convertSolution = fmap (uncurry truth) . Map.assocs

    truth assignment True = assignment
    truth assignment False = negated assignment

-- | We simplify a solution using the fact that attribute assignments are
-- mutually exclusive, which means that, for each attribute, unless we have
-- only inequalities, there is a single equality on that attribute and it
-- implies all other inequalities, which we can remove from the solution.
--
-- >>> let sol = ["x" :==: "1", "x" :!=: "2", "x" :!=: "3"]
-- >>> simplify sol
-- [x = "1"]
simplify :: Solution -> Solution
simplify = isolatePositive <=< groupByAttribute
  where
    isolatePositive :: Solution -> Solution
    isolatePositive assignments = maybe assignments (: []) $ find positive assignments

    groupByAttribute :: Solution -> [Solution]
    groupByAttribute = groupBy (\p q -> attributeName p == attributeName q)

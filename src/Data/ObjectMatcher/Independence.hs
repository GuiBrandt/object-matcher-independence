-- |
-- Module      :  Data.ObjectMatcher.Independence
-- Copyright   :  2021 Guilherme G. Brandt
-- License     :  MIT
-- Maintainer  :  gui.g.brandt@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- = Introduction
--
-- Two 'Data.ObjectMatcher.ObjectMatcher's are said to be /independent/ when
-- there's no object that can be matched simultaneously by both.
--
-- For instance, the matchers @Attribute "x" ["1", "2"]@ and
-- @Attribute "x" ["2", "3"]@ are /not/ independent, because both match the
-- object @{"x": "2"}@.
--
-- This module provides functions for proving whether two object matchers are
-- independent, as well as finding counter-examples when they are not.
module Data.ObjectMatcher.Independence (Result(..), check, maximalIndependentSets') where

import Data.Algorithm.MaximalCliques (getMaximalCliques)
import qualified Data.HashSet as Set
import Data.ObjectMatcher (ObjectMatcher (..))
import Data.ObjectMatcher.Satisfiability (Solution, solve)
import Internal.Utils (pairs)

-- | The result of a call to 'check'. It either confirms that two object
-- matchers are independent or provides counter examples that match both at
-- the same time.
data Result = Independent | CounterExamples [Solution] deriving (Eq, Show)

-- | Checks whether two given object matchers are independent, providing
-- counter examples if not.
--
-- >>> let p = Attribute "x" ["1", "2"]
-- >>> let q = Attribute "x" ["2", "3"]
-- >>> check p q
-- CounterExamples [[x = "2"]]
check :: ObjectMatcher -> ObjectMatcher -> Result
check p q =
  let solutions = solve $ p <> q
   in if null solutions
        then Independent
        else CounterExamples solutions

-- | Calculates the [maximal independent sets](https://en.wikipedia.org/wiki/Maximal_independent_set)
-- on a list of 'Data.ObjectMatcher.ObjectMatcher's, using 'check' to test for
-- independence.
--
-- >>> let p = Attribute "x" ["1"]
-- >>> let q = Attribute "x" ["2"]
-- >>> let r = Attribute "y" ["3"]
-- >>> maximalIndependentSets [p, q, r]
-- [[x = "1",x = "2"],[y = "3"]]
maximalIndependentSets :: [ObjectMatcher] -> [[ObjectMatcher]]
maximalIndependentSets = maximalIndependentSets' check

-- | Calculates the [maximal independent sets](https://en.wikipedia.org/wiki/Maximal_independent_set)
-- on a list of 'Data.ObjectMatcher.ObjectMatcher's from a given independence
-- test function.
maximalIndependentSets' :: (ObjectMatcher -> ObjectMatcher -> Result) -> [ObjectMatcher] -> [[ObjectMatcher]]
maximalIndependentSets' chk ms =
  let independencies = Set.fromList $ do
        (p, q) <- pairs ms
        case chk p q of
          Independent -> [(p, q), (q, p)]
          _ -> []
      edges = curry (`Set.member` independencies)
   in getMaximalCliques edges ms

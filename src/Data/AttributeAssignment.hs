{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Data.AttributeAssignment
-- Copyright   :  2021 Guilherme G. Brandt
-- License     :  MIT
-- Maintainer  :  gui.g.brandt@gmail.com
-- Stability   :  experimental
-- Portability :  portable
module Data.AttributeAssignment (AttributeAssignment (..), attributeName, positive, negated) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | Type of an attribute assignment. An attribute assignment is either an
-- equality (':==:') or an inequality (':!=:') of an attribute to a value.
data AttributeAssignment = String :==: String | String :!=: String
  deriving (Eq, Ord, Generic)

instance Hashable AttributeAssignment

instance Show AttributeAssignment where
  show (name :==: value) = name ++ " = \"" ++ value ++ "\""
  show (name :!=: value) = name ++ " ≠ \"" ++ value ++ "\""

attributeName :: AttributeAssignment -> String
attributeName (name :==: _) = name
attributeName (name :!=: _) = name

-- | Determines whether an assignment is positive (i.e. an equality) or not.
--
-- >>> positive $ "x" :==: "1"
-- True
-- >>> positive $ "x" :!=: "1"
-- False
positive :: AttributeAssignment -> Bool
positive (_ :==: _) = True
positive (_ :!=: _) = False

-- | Negates an attribute assignment, turning equalities into inequalities and
-- vice versa.
--
-- >>> negated $ "x" :!=: "1"
-- x = "1"
negated :: AttributeAssignment -> AttributeAssignment
negated (x :==: y) = x :!=: y
negated (x :!=: y) = x :==: y

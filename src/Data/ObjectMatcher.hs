module Data.ObjectMatcher where

import Data.List (intercalate)

data ObjectMatcher
  = Attribute String [String]
  | Conjunction [ObjectMatcher]
  | Not ObjectMatcher
  deriving (Eq)

instance Show ObjectMatcher where
  show (Attribute name [value]) = name ++ " = \"" ++ value ++ "\""
  show (Attribute name values) = name ++ " ∈ {" ++ intercalate ", " (map (\x -> "\"" ++ x ++ "\"") values) ++ "}"
  show (Conjunction []) = "∅"
  show (Conjunction [matcher]) = show matcher
  show (Conjunction matchers) = intercalate " ∧ " $ show <$> matchers
  show (Not matcher) = "¬(" ++ show matcher ++ ")"

instance Semigroup ObjectMatcher where
  (Conjunction []) <> m = m
  m <> (Conjunction []) = m
  (Conjunction ps) <> (Conjunction qs) = Conjunction $ ps ++ qs
  (Conjunction ms) <> m = Conjunction $ m : ms
  m <> (Conjunction ms) = Conjunction $ m : ms
  p <> q = Conjunction [p, q]

instance Monoid ObjectMatcher where
  mempty = Conjunction []

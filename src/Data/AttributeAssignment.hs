module Data.AttributeAssignment where

data AttributeAssignment = String :==: String | String :!=: String deriving (Eq, Ord)

instance Show AttributeAssignment where
  show (name :==: value) = name ++ " = \"" ++ value ++ "\""
  show (name :!=: value) = name ++ " ≠ \"" ++ value ++ "\""

attributeName :: AttributeAssignment -> String
attributeName (name :==: _) = name
attributeName (name :!=: _) = name

positive :: AttributeAssignment -> Bool
positive (_ :==: _) = True
positive (_ :!=: _) = False

negated :: AttributeAssignment -> AttributeAssignment
negated (x :==: y) = x :!=: y
negated (x :!=: y) = x :==: y

truth :: AttributeAssignment -> Bool -> AttributeAssignment
truth assignment True = assignment
truth assignment False = negated assignment

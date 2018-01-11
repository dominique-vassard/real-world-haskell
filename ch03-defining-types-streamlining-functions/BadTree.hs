module BadTree(nodesAreSame) where

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

-- This doesn't work
-- because we can't match with the same name
-- bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
-- bad_nodesAreSame _            _            = Nothing

-- To solve this problem, we use guards
nodesAreSame :: (Eq a) => Tree a -> Tree a -> Maybe a
nodesAreSame (Node a _ _) (Node b _ _)
  | a == b = Just a
nodesAreSame _ _ = Nothing

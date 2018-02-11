module Partial(isInAny, isInAny2, isInAny3) where
import qualified Data.List as L

isInAny :: (Eq a, Foldable t) => [a] -> t [a] -> Bool
isInAny needle  = any inSequence
    where inSequence s= needle `L.isInfixOf` s

isInAny2 :: (Eq a, Foldable t) => [a] -> t [a] -> Bool
isInAny2 needle = any (\s -> needle `L.isInfixOf` s)

isInAny3 :: (Eq a, Foldable t) => [a] -> t [a] -> Bool
isInAny3 needle = any (`L.isInfixOf` needle)
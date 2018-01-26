module ExercisesFold
  ( groupBy', concat'
  ) where

-- The Data.List module defines a function, groupBy, which has the following type.
-- Use ghci to load the Data.List module and figure out what groupBy does, then write your own implementation using a fold.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f =
  foldl fun []
  where
    fun [] a = [[a]]
    fun acc a =
      let
          (headacc:rest) = reverse acc
          (x:_) = headacc
      in
        if f x a
              then reverse ((headacc ++ [a]):rest)
              else reverse ([a]:headacc:rest)

-- The Prelude function concat concatenates a list of lists into a single list, and has the following type.
concat' :: [[a]] -> [a]
concat' = foldr (++) []

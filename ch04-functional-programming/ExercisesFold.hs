module ExercisesFold
  ( groupBy', concat', takeWhile', takeWhileFold, any', words', unlines'
  ) where


import Data.List (foldl')

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

-- How many of the following Prelude functions can you rewrite using list folds?
--   - any
--    Yes. Better is foldr to allow infinite list treatment
--   - cycle
--    No, can't product infinite list from finite list with only a fold
--   - words
--    Yes. foldl' to use minimum memory (chunk can be quite large)
--   - unlines
--    Yes. foldr for inifinte list
-- For those functions where you can use either foldl' or foldr, which is more appropriate in each case?

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl (\acc a -> acc || f a) False

words' :: String -> [String]
words' s = reverse $ foldl' f [""] s
  where
    f [] _ = []
    f acc ' ' = []:acc
    f (x:xs) l = (x ++ [l]):xs

unlines' :: [String] -> String
unlines' = foldr (\x acc -> x ++ "\n" ++ acc) ""

-- Write your own definition of the standard takeWhile function, first using explicit recursion, then foldr.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = loop []
  where
    loop acc [] = reverse acc
    loop acc (x:xs)
      | f x = loop (x:acc) xs
      | otherwise = loop acc xs

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold f = foldr (\a acc -> if f a then a:acc else []) []
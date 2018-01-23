module Exercises(safeHead, safeTail, safeLast, safeInit, splitWith) where

-- Write your own “safe” definitions of the standard partial list functions, but make sure that yours never fail.
-- As a hint, you might want to consider using the following types.
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (tail (reverse xs))

-- Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type,
-- and splits its input list on every element for which the predicate returns False.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs =
    case span f xs of
      (_, [])         -> []
      (_, [_])        -> []
      ([], _:rest)    -> splitWith f rest
      (valid, _:rest) -> valid: splitWith f rest


-- Using the command framework from the section called “A simple command line framework”,
-- write a program that prints the first word of each line of its input.

-- Write a program that transposes the text in a file.
-- For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
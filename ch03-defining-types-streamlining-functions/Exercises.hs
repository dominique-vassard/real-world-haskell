
module Exercises(length', sum', mean, reverse', toPalindrome, isPalindrome, sortList, intersperse') where
import qualified Data.List as L

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

mean :: Fractional p => [p] -> p
mean [] = 0
mean xs = sum' xs / fromIntegral (length' xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

toPalindrome :: [a] -> [a]
-- toPalindrome [] = []
toPalindrome xs = xs ++ reverse' xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse' xs

sortList :: [[a]] -> [[a]]
sortList = L.sortBy (\a b -> compare (length' a) (length' b))

intersperse' :: String -> [String] -> String
intersperse' _ [] = []
intersperse' _ [x] = x
intersperse' s (x:xs) = x ++ s ++ intersperse' s xs


module Exercises(length', sum', mean, reverse', toPalindrome, isPalindrome, sortList, intersperse', height) where
import qualified Data.List as L

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

mean :: Fractional p => [p] -> p
mean [] = 0
mean xs = sum' xs / fromIntegral (length' xs)

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

toPalindrome :: [a] -> [a]
-- toPalindrome [] = []
toPalindrome xs = xs ++ reverse' xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse' xs

sortList :: [[a]] -> [[a]]
sortList = L.sortBy (\a b -> compare (length' a) (length' b))

intersperse' :: String -> [String] -> String
intersperse' _ []     = []
intersperse' _ [x]    = x
intersperse' s (x:xs) = x ++ s ++ intersperse' s xs


-- Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree.
-- The height is the largest number of hops from the root to an Empty.
-- For example, the tree Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

height :: Tree a -> Int
height Empty          = 0
height (Node _ lt rt) = 1 + max (height lt)  (height rt)

-- Consider three two-dimensional points a, b, and c.
-- If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line.
-- Define a Direction data type that lets you represent these possibilities.
-- Write a function that calculates the turn made by three 2D points and returns a Direction.
data Point = Point (Int, Int)
data Direction = Left
                | Straight
                | Right

-- getDirection :: [a] -> Direction
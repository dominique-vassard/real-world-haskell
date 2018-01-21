module Map(square, upperCase, square', upperCase', myMap) where

import           Data.Char (toUpper)

square :: [Double] -> [Double]
square []     = []
square (x:xs) = x*x : square xs

square' :: [Double] -> [Double]
square' xs = map squareOne xs
    where squareOne x = x * x

upperCase :: String -> String
upperCase []     = []
upperCase (x:xs) = toUpper x : upperCase xs

upperCase' :: String -> String
upperCase' xs = map toUpper xs

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs)= f x : myMap f xs
myMap _ _ = []
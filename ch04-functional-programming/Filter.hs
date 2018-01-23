module Filter (oddList, myFilter) where

oddList :: [Int] -> [Int]
oddList (x:xs)
    | odd x = x : oddList xs
    | otherwise = oddList xs
oddList _ = []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
    | f x = x : myFilter f xs
    | otherwise = myFilter f xs
module Sum(mySum, mySumAcc, foldlSum, niceSum) where

mySum :: [Int] -> Int
mySum [] = 0
mySum [x] = x
mySum (x:y:xs) = x + y + mySum xs

mySumAcc :: [Int] -> Int
mySumAcc xs = helper 0 xs
    where
        helper acc [] = acc
        helper acc (b:bs) =  helper (acc + b) bs


foldlSum :: [Int] -> Int
foldlSum xs = foldl step 0 xs
    where step acc x = acc + x

niceSum :: [Int] -> Int
niceSum xs = foldl (+) 0 xs

-- foldl (+) 0 (1:2:3:[])
--           == foldl (+) (0 + 1)             (2:3:[])
--           == foldl (+) ((0 + 1) + 2)       (3:[])
--           == foldl (+) (((0 + 1) + 2) + 3) []
--           ==           (((0 + 1) + 2) + 3)
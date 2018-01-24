module IntParse(asInt) where

import           Data.Char (digitToInt, isDigit)

asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) =
    let
        acc' = acc * 10 + digitToInt x
    in
        loop acc' xs

-- Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the asInt function
-- ghci> asInt_fold "101"
-- 101
-- ghci> asInt_fold "-31337"
-- -31337
-- ghci> asInt_fold "1798"
-- 1798

-- Extend your function to handle the following kinds of exceptional conditions by calling error
-- ghci> asInt_fold ""
-- 0
-- ghci> asInt_fold "-"
-- 0
-- ghci> asInt_fold "-3"
-- -3
-- ghci> asInt_fold "2.7"
-- *** Exception: Char.digitToInt: not a digit '.'
-- ghci> asInt_fold "314159265358979323846"
-- 564616105916946374
asIntFold :: String -> Int
asIntFold [] = error "String too short."
asIntFold "-" = error "No digit found."
asIntFold ('-':xs) = -asIntFold xs
asIntFold xs = foldl facc 0 xs
  where
    toDigit a
      | isDigit a = digitToInt a
      | otherwise = error ("Non-digit character found: [" ++ [a] ++ "]")
    facc acc x =
      let
        acc' = acc * 10 + toDigit x
      in
        if acc' < acc then error "Int overflow" else acc'


-- The asInt_fold function uses error, so its callers cannot handle errors. Rewrite it to fix this problem.
type ErrorMessage = String
asIntEither :: String -> Either ErrorMessage Int
asIntEither [] = Left "String too short."
asIntEither "-" = Left "No digit found."
asIntEither ('-':xs) =
  case asIntEither xs of
    Left err -> Left err
    Right result -> Right (-result)
asIntEither xs = foldl facc (Right 0) xs
  where
    facc (Left err) _ = Left err
    facc (Right acc) x
      | isDigit x =
        let
          acc' = acc * 10 + digitToInt x
        in
          if acc' < acc then Left "Int overflow" else Right acc'
      | otherwise = Left ("Non-digit character found: [" ++ [x] ++ "]")

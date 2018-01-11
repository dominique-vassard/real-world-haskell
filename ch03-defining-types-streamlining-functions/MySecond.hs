module MySecond
  ( mySecond
  , safeSecond
  , tidySecond
  ) where

mySecond :: [a] -> a
mySecond xs =
  if null (tail xs)
    then error "List too short"
    else head (tail xs)

safeSecond :: [a] -> Maybe a
safeSecond []     = Nothing
safeSecond [_]    = Nothing
safeSecond (_:xs) = Just (head xs)


tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

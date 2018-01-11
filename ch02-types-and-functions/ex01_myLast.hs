module Test
  ( myLast
  ) where

-- main = putStrLn "Hello World"
myLast :: [a] -> a
myLast []     = error "Empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

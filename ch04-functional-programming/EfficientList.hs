module EfficientList
  ( myDumbExample
  , mySmartExample
  ) where

-- not good as length is not known, it required to go through the entire list
-- which can be infinite...
myDumbExample :: String -> Char
myDumbExample xs =
  if length xs > 0
    then head xs
    else 'Z'

-- Idiomatic way to do it
mySmartExample :: String -> Char
mySmartExample xs =
  if null xs
    then head xs
    else 'Z'

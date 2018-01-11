module NestedLets(foo, bar, quux) where

-- Nested let
foo::Int
foo = let a = 1
      in let b = 2
         in a + b


-- Possible but not wise
-- Shadowing: inner x is shadowing outer x
-- IDE raises:
-- Warning: This binding for ‘x’ shadows the existing binding
bar::(String, Int)
bar = let x = 1
      in ((let x = "foo" in x), x)

quux:: a -> String
quux a = let a = "foo"
         in a ++ "eek!"

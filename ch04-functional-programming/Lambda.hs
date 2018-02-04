module Lambda (safeHead, unsafeHead) where

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

-- THis will explode beacuse all patterns are not cover
-- namely: unsafeHead []
unsafeHead :: [a] -> a
unsafeHead = \(x:_) -> x
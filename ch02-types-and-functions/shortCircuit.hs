-- Haskell is lazy: expressions are computed only when needed
-- Then if a is evaluate to True, else won't be computed
newOr a b = if a then a else b
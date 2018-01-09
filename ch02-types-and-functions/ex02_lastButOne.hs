lastButOne :: [a] -> a
lastButOne [] = error "Empty list"
lastButOne (x:[]) = error "Non computable"
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs

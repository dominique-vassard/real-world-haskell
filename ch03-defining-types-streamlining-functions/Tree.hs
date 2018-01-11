module Tree(myTree, myAltTreeEmpty, myAltTreeSomething) where

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

data AltTree a = AltNode a (Maybe (AltTree a)) (Maybe (AltTree a))

myTree :: Tree String
myTree = Node "parent" (Node "left" Empty Empty) (Node "right" Empty Empty)

myAltTreeEmpty :: AltTree String
myAltTreeEmpty =
  AltNode "parent" Nothing Nothing


myAltTreeSomething :: AltTree String
myAltTreeSomething =
  AltNode "parent" (Just (AltNode "left" Nothing Nothing)) (Just (AltNode "right" Nothing Nothing))

-- other solution


-- data MyTree a = MyTree (Maybe (a, MyTree a, MyTree a)) deriving (Show)

-- empty = MyTree Nothing
-- left_node = MyTree (Just("left_node", empty, empty))
-- right_node = MyTree (Just("right_node", empty, empty))
-- parent = MyTree (Just("parent", left_node, right_node))


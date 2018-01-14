
module Exercises(length', sum', mean, reverse', toPalindrome, isPalindrome, sortList, intersperse', height, getDirection, getDirections, Point, Direction(..),
    filter', comparePoint, sortPoints, grahamScan, reducePointList, test, testData, compareAngle, grahamScan') where
import qualified Data.List as L

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

mean :: Fractional p => [p] -> p
mean [] = 0
mean xs = sum' xs / fromIntegral (length' xs)

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

toPalindrome :: [a] -> [a]
-- toPalindrome [] = []
toPalindrome xs = xs ++ reverse' xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse' xs

sortList :: [[a]] -> [[a]]
sortList = L.sortBy (\a b -> compare (length' a) (length' b))

intersperse' :: String -> [String] -> String
intersperse' _ []     = []
intersperse' _ [x]    = x
intersperse' s (x:xs) = x ++ s ++ intersperse' s xs


-- Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree.
-- The height is the largest number of hops from the root to an Empty.
-- For example, the tree Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

height :: Tree a -> Int
height Empty          = 0
height (Node _ lt rt) = 1 + max (height lt)  (height rt)

-- Consider three two-dimensional points a, b, and c.
-- If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line.
-- Define a Direction data type that lets you represent these possibilities.
-- Write a function that calculates the turn made by three 2D points and returns a Direction.
data Point = Point Int Int
    deriving (Show)
data Direction = TurnLeft
                | GoStraight
                | TurnRight
    deriving (Show)

-- use cross product: u x v = ||u|| ||b|| sin(theta)
-- with theta = angle between a and b
-- considering vector from a to b (ab) and vector fomr b to c (bc)
-- if ab x bc == 0 then Straight
-- if ab x bc < 0 then Left
-- if ab x bc > 0 then Right
-- considering vector u as (u1, u2) and vector v as (v1, v2)
-- cross product is : (u1 * v2)
-- Or from Graham scan wikipedia
-- Again, determining whether three points constitute a "left turn" or a "right turn" does not require computing the actual angle between the two line segments,
-- and can actually be achieved with simple arithmetic only.
-- For three points P 1 = ( x 1 , y 1 ), P 2 = ( x 2 , y 2 ) and P 3 = ( x 3 , y 3 ) ,
-- compute the z-coordinate of the cross product of the two vectors P 1 P 2 and P 1 P 3,
-- which is given by the expression ( x 2 − x 1 ) ( y 3 − y 1 ) − ( y 2 − y 1 ) ( x 3 − x 1 ) .
-- If the result is 0, the points are collinear;
-- if it is positive, the three points constitute a "left turn" or counter-clockwise orientation,
-- otherwise a "right turn" or clockwise orientation (for counter-clockwise numbered points).

getDirection :: Point -> Point -> Point -> Direction
getDirection (Point x1 y1) (Point x2 y2) (Point x3 y3)
        | res == 0 = GoStraight
        | res < 0 = TurnRight
        | otherwise = TurnLeft
    where
            res = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 -x1)


-- Define a function that takes a list of 2D points and computes the direction of each successive triple.
-- Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e].
-- Your function should return a list of Direction.
getDirections :: [Point] -> [Direction]
getDirections [] = []
getDirections [_] = []
getDirections [_,_]= []
getDirections (p1:p2:p3:ps) = getDirection p1 p2 p3:getDirections (p2:p3:ps)

-- Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of 2D points.
-- You can find good description of what a convex hull. is, and how the Graham scan algorithm should work, on Wikipedia
-- From wikipedia
-- Graham's scan is a method of finding the convex hull of a finite set of points in the plane with time complexity O(n log n).
-- It is named after Ronald Graham, who published the original algorithm in 1972.
-- [1] The algorithm finds all vertices of the convex hull ordered along its boundary. It uses a stack to detect and remove concavities in the boundary efficiently.

-- The first step in this algorithm is to find the point with the lowest y-coordinate.
-- If the lowest y-coordinate exists in more than one point in the set, the point with the lowest x-coordinate out of the candidates should be chosen.
-- Call this point P. This step takes O(n), where n is the number of points in question.

-- Next, the set of points must be sorted in increasing order of the angle they and the point P make with the x-axis.
-- Any general-purpose sorting algorithm is appropriate for this, for example heapsort (which is O(n log n))

-- The algorithm proceeds by considering each of the points in the sorted array in sequence.
-- For each point, it is first determined whether traveling from the two points immediately preceding this point constitutes making a left turn or a right turn.
-- If a right turn, the second-to-last point is not part of the convex hull, and lies 'inside' it.
-- The same determination is then made for the set of the latest point and the two points that immediately precede the point found to have been inside the hull,
-- and is repeated until a "left turn" set is encountered, at which point the algorithm moves on to the next point in the set of points in the sorted array
-- minus any points that were found to be inside the hull; there is no need to consider these points again.
-- (If at any stage the three points are collinear, one may opt either to discard or to report it,
--     since in some applications it is required to find all points on the boundary of the convex hull.)

comparePoint :: Point -> Point -> Ordering
comparePoint (Point x1 y1) (Point x2 y2)
    | y1 > y2 = GT
    | y1 < y2 = LT
    | y1 == y2 && x1 > x2 = GT
    | y1 == y2 && x1 < x2 = LT
    | otherwise = EQ

reducePointList :: [Point] -> [Point]
reducePointList [] = []
reducePointList [_] = []
reducePointList [_,_]= []
reducePointList full@(p1:p2:p3:ps) =
    case getDirection p3 p2 p1 of
        TurnLeft -> reverse full
        TurnRight -> reducePointList (p1:p3:ps)
        GoStraight -> reverse full


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' fn (x:xs)
    | fn x = x:filter' fn xs
    | otherwise = filter' fn xs

sortPoints :: [Point] -> [Point]
sortPoints = L.sortBy comparePoint


compareAngle :: Point -> Point -> Ordering
compareAngle p1 p2 =
    case getDirection (Point 0 0) p1 p2 of
        TurnLeft -> LT
        TurnRight -> GT
        GoStraight -> EQ

grahamScan' :: [Point] -> [Point]
grahamScan' ps =
    let
        (minPoint:rest) = L.sortBy comparePoint ps
    in
        grahamScan $ minPoint:L.sortBy compareAngle rest

grahamScan :: [Point] -> [Point]
grahamScan [] = []
grahamScan [p1] = [p1]
grahamScan [p1, p2]= [p1, p2]
grahamScan (p1:p2:p3:ps) =

    case getDirection p1 p2 p3 of
        TurnLeft -> p1:grahamScan (p2:p3:ps)
        TurnRight -> grahamScan (reducePointList (reverse (p1:p3:ps)))
        GoStraight -> p1:grahamScan (p2:p3:ps)

-- let test_data = [Point 4 3, Point 5 1, Point 4 1, Point 1 2, Point 5 2, Point 2 1, Point 3 5, Point 2 3]
-- let sorted_data = sortPoints test_data

testData :: [Point]
testData = [Point 4 3, Point 5 1, Point 4 1, Point 1 2, Point 5 2, Point 2 1, Point 3 5, Point 2 3]

test :: [Point]
test =
    let
        test_data = [Point 4 3, Point 5 1, Point 4 1, Point 1 2, Point 5 2, Point 2 1, Point 3 5, Point 2 3]
    in
        grahamScan' test_data
module WeekThree where

import WeekOne
import Data.Maybe

{-
    26. Insert Interval

    You are given an ACS-sorted array of non-overlapping intervals intervals 
    Insert a new intervals that reminds the current structure

    Requirement: 
    1. still ASC-sorted array
    2. No over-lapping intervals

    insertInterval [(4,6),(8,10)] (12,14) -> [(4,6),(8,10),(12,14)]
    insertInterval [(4,6),(8,10)] (1,2) -> [(1,2),(4,6),(8,10)]
    insertInterval [(4,6),(8,10)] (8,9) -> [(4,6),(8,10)]
    insertInterval [(4,6),(8,10)] (3,10) -> [(3,10)]
    insertInterval [(4,6),(8,10)] (1,5) -> [(1,6),(8,10)]
    insertInterval [(4,6),(8,10)] (9,12) -> [(4,6),(8,12)]
-}
insertInterval :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
insertInterval [] interval = [interval]
insertInterval ((x1, x2):xs) (insert1, insert2)
    | insert1 >= x1 && insert2 <= x2 = (x1, x2):xs
    | insert2 < x1 = (insert1, insert2): (x1, x2): xs
    | insert1 > x2 = (x1, x2): insertInterval xs (insert1, insert2)
    | otherwise = if insert1 >= x1 
                  then (x1, right): rest
                  else (insert1, right): rest
    where (right, rest) = lookForRight ((x1, x2): xs) insert2

lookForRight :: [(Int, Int)] -> Int -> (Int, [(Int, Int)])
lookForRight [] right = (right, [])
lookForRight ((x1, x2):xs) right 
    | right < x1 = (right, xs)
    | right < x2 = (x2, xs)
    | otherwise = lookForRight xs right

{-
    27. 01 Matrix

    Given an m x n binary matrix mat, 
    return the distance of the nearest 0-value cell for each cell.

    The distance of the nearest 0 for a cell in the binary matrix is the minimum 
    number of steps needed to reach the nearest 0-valued cell from that cell, 
    where a step is a movement from one adjacent cell to another (not diagonally).
    
    The distance between two adjacent cells is 1.

    Test Cases: 
    distance mat1
    distance mat2
    distance mat3
-}
data Binary = One | Zero 

instance Eq Binary where
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

type BinaryMat = [[Binary]]
type Coord = (Int, Int)
type Dist = Maybe Int
type DistMat = [[Dist]]

mat1 = [[Zero,Zero,Zero,Zero],
       [Zero,One ,Zero,Zero],
       [One ,One ,One ,Zero],
       [One ,One ,One ,One ]]

mat2 = [[One,Zero],
        [One,One ]]

mat3 = [[One,One],
        [One,One ]]
       
distance :: BinaryMat -> DistMat
distance binaryMat 
    | isZeroExist = distanceAux binaryMat [(0,0)] initDistMat
    | otherwise = initDistMat
    where isZeroExist = foldl (\acc x -> acc || x == Zero) False (concat binaryMat)
          initDistMat = fillNothing binaryMat
{-
    Find distance matrix base on binary matrix and coordinates
    the coordinates use a queue do a breadth-first visit

    BinaryMat: binary matrix
    [Coord]  : potential coordinates, 
    DistMat  : matrix of distance
-} 
distanceAux :: BinaryMat -> [Coord] -> DistMat -> DistMat
distanceAux _ [] distMat = distMat
distanceAux binaryMat ((r,c):xs) distMat
    -- distance already calculate
    | isJust(distMat!!r!!c)= distanceAux binaryMat xs distMat
    -- distance is zero
    | binaryMat!!r!!c == Zero = distanceAux binaryMat (xs ++ validNextCoords ) (paint distMat (r,c) (Just 0))
    -- distance can be calculated
    | isNextToValue = distanceAux binaryMat (xs ++ validNextCoords) (paint distMat (r,c) (Just (minValue+1)))
    -- distance cannot be calculated at the moment
    | otherwise = distanceAux binaryMat (xs ++ validNextCoords ++ [(r,c)]) distMat
    where maxRow = length distMat
          maxCol = length $ head distMat
          validNextCoords = validNext (maxRow, maxCol) [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]
          isNextToValue = foldl (\acc (r,c) -> acc || isJust (distMat!!r!!c)) False validNextCoords
          minValue = minimum (nextValues distMat validNextCoords)

-- get values from valid cells
nextValues :: DistMat -> [Coord] -> [Int]
nextValues _ [] = []
nextValues distMat ((r,c):xs)
    | distMat!!r!!c == Nothing = nextValues distMat xs
    | otherwise = v: nextValues distMat xs
    where Just v = distMat!!r!!c

-- check valid cells for a breadth-first visited
validNext :: Coord -> [Coord] ->[Coord]
validNext _ [] = []
validNext (maxRow, maxCol) ((r,c):xs)
    | r >= maxRow || c >= maxCol || r < 0 || c < 0 = validNext (maxRow, maxCol) xs
    | otherwise = (r,c): validNext (maxRow, maxCol) xs

-- fill the initial distance matrix with Nothing
fillNothing :: [[a]] -> DistMat
fillNothing [] = []
fillNothing (x:xs) = (fillNothingAux x): fillNothing xs

fillNothingAux :: [a] -> [Dist]
fillNothingAux [] = []
fillNothingAux (x: xs) = (Nothing): fillNothingAux xs

{-
    28. K Closest Points to Origin

    Given an array of coordinates (Int,Int) and an integer k, return the top k 
    closest coordinates based on its distance from the origin (0, 0).

    The distance calculated based on the Euclidean distance.

    kCloset 1 [(1,3),(-2,2)]
    kCloset 2 [(3,3),(5,-1),(-2,4)]
-}
one :: Int
one = 1

-- calculate all the distance, then sort them based on distance, 
-- then select top k elements
kCloset :: Nat -> [Coord] -> [Coord]
kCloset k xs = map (\(x,_) -> x) topKs
    where topKs = take k $ kClosetAux xs

kClosetAux :: [(Coord)] -> [(Coord, Int)]
kClosetAux xs = sortBy (\(_, d1) (_, d2) -> compare d1 d2) xsWithDist
    where xsWithDist = map (\(x,y) -> ((x,y), (x^2 + y^2))) xs

-- use distance as the key, and add coords to values.
-- choice top k key, then find top k elements

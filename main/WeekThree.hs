module WeekThree where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import WeekOne hiding (convToDict, convToDictHelper)

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
insertInterval ((x1, x2) : xs) (insert1, insert2)
  | insert1 >= x1 && insert2 <= x2 = (x1, x2) : xs
  | insert2 < x1 = (insert1, insert2) : (x1, x2) : xs
  | insert1 > x2 = (x1, x2) : insertInterval xs (insert1, insert2)
  | otherwise =
    if insert1 >= x1
      then (x1, right) : rest
      else (insert1, right) : rest
  where
    (right, rest) = lookForRight ((x1, x2) : xs) insert2

lookForRight :: [(Int, Int)] -> Int -> (Int, [(Int, Int)])
lookForRight [] right = (right, [])
lookForRight ((x1, x2) : xs) right
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
  One == One = True
  _ == _ = False

type BinaryMat = [[Binary]]

type Coord = (Int, Int)

type Dist = Maybe Int

type DistMat = [[Dist]]

mat1 =
  [ [Zero, Zero, Zero, Zero],
    [Zero, One, Zero, Zero],
    [One, One, One, Zero],
    [One, One, One, One]
  ]

mat2 =
  [ [One, Zero],
    [One, One]
  ]

mat3 =
  [ [One, One],
    [One, One]
  ]

distance :: BinaryMat -> DistMat
distance binaryMat
  | isZeroExist = distanceAux binaryMat [(0, 0)] initDistMat
  | otherwise = initDistMat
  where
    isZeroExist = foldl (\acc x -> acc || x == Zero) False (concat binaryMat)
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
distanceAux binaryMat ((r, c) : xs) distMat
  -- distance already calculate
  | isJust (distMat !! r !! c) = distanceAux binaryMat xs distMat
  -- distance is zero
  | binaryMat !! r !! c == Zero = distanceAux binaryMat (xs ++ validNextCoords) (paint distMat (r, c) (Just 0))
  -- distance can be calculated
  | isNextToValue = distanceAux binaryMat (xs ++ validNextCoords) (paint distMat (r, c) (Just (minValue + 1)))
  -- distance cannot be calculated at the moment
  | otherwise = distanceAux binaryMat (xs ++ validNextCoords ++ [(r, c)]) distMat
  where
    maxRow = length distMat
    maxCol = length $ head distMat
    validNextCoords = validNext (maxRow, maxCol) [(r -1, c), (r + 1, c), (r, c -1), (r, c + 1)]
    isNextToValue = foldl (\acc (r, c) -> acc || isJust (distMat !! r !! c)) False validNextCoords
    minValue = minimum (nextValues distMat validNextCoords)

-- get values from valid cells
nextValues :: DistMat -> [Coord] -> [Int]
nextValues _ [] = []
nextValues distMat ((r, c) : xs)
  | distMat !! r !! c == Nothing = nextValues distMat xs
  | otherwise = v : nextValues distMat xs
  where
    Just v = distMat !! r !! c

-- check valid cells for a breadth-first visited
validNext :: Coord -> [Coord] -> [Coord]
validNext _ [] = []
validNext (maxRow, maxCol) ((r, c) : xs)
  | r >= maxRow || c >= maxCol || r < 0 || c < 0 = validNext (maxRow, maxCol) xs
  | otherwise = (r, c) : validNext (maxRow, maxCol) xs

-- fill the initial distance matrix with Nothing
fillNothing :: [[a]] -> DistMat
fillNothing = map fillNothingAux

fillNothingAux :: [a] -> [Dist]
fillNothingAux = map (const Nothing)

{-
  28. K Closest Points to Origin

  Given an array of coordinates (Int,Int) and an integer k, return the top k
  closest coordinates based on its distance from the origin (0, 0).

  The distance calculated based on the Euclidean distance.

  kCloset 1 [(1,3),(-2,2)]        -> [(-2,2)]
  kCloset 2 [(3,3),(5,-1),(-2,4)] -> [(3,3),(-2,4)]
-}

-- calculate all the distance, then sort them based on distance,
-- then select top k elements, O(n log(n)), O(n) in space
kCloset :: Nat -> [Coord] -> [Coord]
kCloset k xs = map fst topKs
  where
    topKs = take k $ kClosetAux xs

{-
  improvement:
  in this case we do not necessary to calculate the square root of (x^2 + y^2).
  Since we use compare, and all result be square root, by transitivity,
  all the sort order should remain the same with just (x^2 + y^2)
-}
kClosetAux :: [Coord] -> [(Coord, Float)]
kClosetAux xs = sortBy (\(_, d1) (_, d2) -> compare d1 d2) xsWithDist
  where
    xsWithDist = map (\(x, y) -> ((x, y), distance x y)) xs
    distance x y = sqrt (fromIntegral x ** 2 + fromIntegral y ** 2)

-- use distance as the key, and add coords to values.
-- choice top k key, then find top k elements, O(n) in time, O(n) in space
kCloset' :: Nat -> [Coord] -> [Coord]
kCloset' k xs = take k (concatVal topKsPot [] dict)
  where
    dict = convToDict xs
    topKsPot = take k (M.keys dict)

concatVal :: Ord a => [a] -> [b] -> M.Map a [b] -> [b]
concatVal [] acc _ = acc
concatVal (k : ks) acc dict = concatVal ks (acc ++ v) dict
  where
    Just v = M.lookup k dict

convToDict xs = convToDictHelper xs M.empty mySqrt

-- generic method convert a list to a dictionary
convToDictHelper :: Ord k => [a] -> M.Map k [a] -> (a -> k) -> M.Map k [a]
convToDictHelper [] dict _ = dict
convToDictHelper (x : xs) dict f
  | f x `M.member` dict = convToDictHelper xs (M.insertWith (++) (f x) [x] dict) f
  | otherwise = convToDictHelper xs (M.insert (f x) [x] dict) f

mySqrt (x, y) = sqrt (fromIntegral x ** 2 + fromIntegral y ** 2)

{-
  29. Longest Substring Without Repeating Characters

  Given a string s, find the length of the longest substring without repeating
  characters.

  Test Cases:

  longestSubStr "abcabcbb"    -> 3, because "abc"
  longestSubStr "bbbbb"       -> 1, because "b"
  longestSubStr "pwwkewc"     -> 4, because "wke"
-}
longestSubStr :: String -> Nat
longestSubStr xs = longestSubStrAux xs 0

-- compare all the length of non-repeated string in each branch
longestSubStrAux :: String -> Nat -> Nat
longestSubStrAux [] m = m
longestSubStrAux xs m = longestSubStrAux (tail xs) (max (longestSubStrLen xs M.empty) m)

-- calculate the length of non-repeated string in this brach
longestSubStrLen :: String -> M.Map Char Nat -> Nat
longestSubStrLen [] _ = 0
longestSubStrLen (x : xs) dict
  | x `M.member` dict = 0
  | otherwise = 1 + longestSubStrLen xs (M.insert x 1 dict)

{-
  30. 3Sum

  Given an array [Int], return all tuple [(array!!i, array!!j, array!!k)]
  such that  i != j, i != k, and j != k, and i+j+k == 0.

  Notice that the solution set must not contain duplicate triplets.

  (a variety of the two sum question the problem can be solved with the two sum solution.)

  Test Cases:

  threeSum [-1,0,1,2,-1,-4]   -> [(-1,-1,2),(-1,0,1)]
  threeSum [0,1,1]            -> []
  threeSum [0,0,0]            -> [(0,0,0)]
-}
threeSum :: [Int] -> [(Int, Int, Int)]
threeSum [] = []
threeSum (x : xs) =
  case twoSums of
    [] -> threeSum xs
    xs' -> map (\(y, z) -> (x, y, z)) xs' ++ threeSum xs
  where
    mirror = negate x
    twoSums = twoPair xs mirror

{-
  31. Binary Tree Level Order Traversal

  Given the root of a binary tree,
  return the level order traversal of its nodes' values.

  Note:
  the level order traversal is a traversal base on the level of nodes

  Test Cases:

  levelOrderOf tree1 -> [[1],[2,3],[4]]
  levelOrderOf tree3 -> [[6],[2,8],[0,4,7,9],[3,5]]
-}
levelOrderOf :: BiTree a -> [[a]]
levelOrderOf tr = levelOrderOfAux [tr]

levelOrderOfAux :: [BiTree a] -> [[a]]
levelOrderOfAux xs =
  case levelOrders of
    [] -> []
    _ -> valOfTrees xs : levelOrderOfAux nextXs
  where
    levelOrders = valOfTrees xs
    nextXs = nextLevel xs

valOfTrees :: [BiTree a] -> [a]
valOfTrees [] = []
valOfTrees (Empty' : xs) = valOfTrees xs
valOfTrees (Node' a _ : xs) = a : valOfTrees xs

nextLevel :: [BiTree a] -> [BiTree a]
nextLevel [] = []
nextLevel (Empty' : xs) = nextLevel xs
nextLevel (Node' _ (left, right) : xs) = left : right : nextLevel xs

{-
  32. Clone Graph

  Given a reference of a node in a connected undirected graph.

  Return a deep copy (clone) of the graph.
-}
graph1 = Node'' 1 [Node'' 2 [Node'' 5 [], Node'' 6 []], Node'' 3 [], Node'' 4 []]

-- not good to represent a graph, better use nodes and edges
data Graph a = Node'' a [Graph a] deriving (Show)

clone :: Graph a -> Graph a
clone (Node'' v []) = Node'' v []
clone (Node'' v xs) = Node'' v (map clone xs)

{-
  33. Evaluate Reverse Polish Notation

  You are given an array of strings tokens that represents an arithmetic
  expression in a Reverse Polish Notation.

  Evaluate the expression. Return an integer that represents the value of the
  expression

  Reverse Polish Notation aka simply postfix notation
  prefix:     + 3 4
  infix:      3 + 4
  postfix:    3 4 +

  Test Cases:

  evalPostfix ["2","1","+","3","*"] -> 9
  evalPostfix ["4","13","5","/","+"] -> 6
  evalPostfix ["10","6","9","3","+","-11","*","/","*","17","+","5","+"] -> 12
-}
operator :: Integral a => String -> a -> a -> a
operator op
  | op == "+" = (+)
  | op == "-" = (-)
  | op == "*" = (*)
  | op == "/" = div
  | otherwise = error "Unrecognized operator"

operators = ["+", "-", "*", "/"]

-- iteration version
evalPostfix :: [String] -> Int
evalPostfix xs = evalPostfixAux xs []

evalPostfixAux :: [String] -> [String] -> Int
evalPostfixAux [x] _ = strToInt x
evalPostfixAux (x1 : x2 : op : xs) pre
  | op `elem` operators = evalPostfixAux newStr []
  | otherwise = evalPostfixAux (x2 : op : xs) (pre ++ [x1])
  where
    arithmeticOp = operator op
    value = strToInt x1 `arithmeticOp` strToInt x2
    newStr = pre ++ show value : xs
evalPostfixAux _ _ = error "Invalid arithmetic expression"

strToInt :: String -> Int
strToInt xs = read xs :: Int

-- stack version
evalPostfix' :: [String] -> Int
evalPostfix' xs = evalPostfix'Aux xs []

evalPostfix'Aux :: [String] -> [String] -> Int
evalPostfix'Aux [] st = strToInt $ head st
evalPostfix'Aux (x : xs) st
  | x `elem` operators = evalPostfix'Aux xs newStack
  | otherwise = evalPostfix'Aux xs (x : st)
  where
    arithmeticOp = operator x
    left = strToInt (head (tail st))
    right = strToInt (head st)
    newStack = show (left `arithmeticOp` right) : tail (tail st)

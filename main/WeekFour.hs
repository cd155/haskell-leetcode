module WeekFour where

import Data.List
import WeekOne (BiTree (Empty', Node'), findAreaInColor)

{-
  34. Course Schedule

  Given a list of class you have to take, and prerequisites.

  1. Check wether you can finish all courses.
  2. List a path how to finish all course.

  The same question in
  https://github.com/cd155/algorithm-design-with-haskell/blob/71661bb4fa30663da215351bfaf55cd768952af7/src/Graph.hs#L64

  Test Cases:
    1. Check wether you can finish all courses
      ( assume it is just one connected graph )
      isCycleGraph $ g preNonCycle1 -> False
      isCycleGraph $ g preNonCycle2 -> False
      isCycleGraph $ g preCycle1    -> True
      isCycleGraph $ g preCycle2    -> True
  
    2. List a path how to finish all course
      ( assume there is no cycle in the graph )
      topoSort $ g preNonCycle1 = "febadc"
      topoSort $ g preNonCycle2 = "fecbad"
-}

g x = (classes, x)
classes = ['a','b','c','d','e','f']
preNonCycle1 = [('a','d'), ('f','b'), ('b','d'), ('f','a'), ('d','c')]
preNonCycle2 = [('a','d')]
preCycle1 = [('a','b'), ('b','c'), ('c','a'), ('a','f'), ('f','d'), ('e','a')]
preCycle2 = [('a','a')]

-- Use number to represent class
type Class = Char

type Prereq = (Class, Class)

type Graph = ([Class], [Prereq])

-- Visit all node in a undirected graph
undirDepthFirst :: Graph -> [Class]
undirDepthFirst ([],_) = []
undirDepthFirst (v,e) = undirdepthFirstAux e [head v] []

{-
  (x:xs): a stack track the order of future visiting nodes
  visited: a list check which node be visited
-}
undirdepthFirstAux :: [Prereq] -> [Class] -> [Class] -> [Class]
undirdepthFirstAux _ [] _ = []
undirdepthFirstAux ps (x:xs) visited = x: undirdepthFirstAux ps (adjacencies ++ xs) (x:visited) 
    where adjacencies = filter (\z -> z `notElem` visited && z `notElem` (x:xs)) possClasses
          possClasses = map (\(x',y') -> if x' == x then y' else x') possEdges
          possEdges   = filter (\(x'',y'') -> x'' == x || y'' == x) ps

-- Detect cycle in a direct graph
isCycleGraph :: Graph -> Bool
isCycleGraph ([],_) = False
isCycleGraph (v,e) = isCycleGraphAux e (head v) [head v]

isCycleGraphAux :: [Prereq] -> Class -> [Class] -> Bool
isCycleGraphAux ps cur path = 
  case adjacencies of
    [] -> False
    xs -> foldl (||) False 
      (map (\x -> if x `elem` path then True else isCycleGraphAux ps x (path++[x])) xs)
  where adjacencies = map snd (filter (\(x',_) -> x' == cur) ps)

-- Sort the graph topologically
topoSort :: Graph -> [Class]
topoSort ([],_) = []
topoSort (v,e) = topoSortWeaver (v,e) []

-- Track all nodes be visited
topoSortWeaver :: Graph -> [Class] -> [Class]
topoSortWeaver (v,e) visited
  | null diffs = visited
  | otherwise = topoSortWeaver (v,e) newVisited
  where newVisited = topoSortAux e [head diffs] visited
        diffs = v \\ visited 

-- Create a partial topological order for the selected node
topoSortAux :: [Prereq] -> [Class] -> [Class] -> [Class]
topoSortAux _ [] visited = visited
topoSortAux ps (x:xs) visited =
  case adjacencies of
    [] -> topoSortAux ps xs (x:visited)
    _  -> topoSortAux ps (adjacencies ++ (x:xs)) (visited)
  where adjacencies = filter (\z -> z `notElem` visited && z `notElem` (x:xs)) possClasses
        possClasses = map snd (filter (\(x',_) -> x' == x) ps)

{-
  35. Implement Trie (Prefix Tree)

  A trie (pronounced as "try") or prefix tree is a tree data structure 
  used to efficiently store and retrieve keys in a dataset of strings.
  There are various applications of this data structure, such as autocomplete
  and spellchecker.

  Implement the Trie class:

  - insert(String word) Inserts the string word into the trie.
  - boolean search(String word) 
    Returns true if the string word is in the trie 
    (i.e., was inserted before), and false otherwise.
  - boolean startsWith(String prefix) 
    Returns true if there is a previously inserted string word that has the prefix prefix, and false otherwise.
  
  Test Case:
  1. insert "many", "my", "lie", "a" into emptyTrie
    insertTrie "a" (insertTrie "lie" (insertTrie "my" (insertTrie "many" emptyTrie)))  
      ->
    [Node 'a' [Empty],
     Node 'l' [Node 'i' [Node 'e' [Empty]]],
     Node 'm' [Node 'y' [Empty],
               Node 'a' [Node 'n' [Node 'y' [Empty]]]
              ]
    ]

  2. search "man" trie -> False
     startsWith "man" trie -> True
-}

data Trie a = Empty | Start [Trie a] | Node a [Trie a] deriving (Show)

instance Eq a => Eq (Trie a) where
  Empty == Empty = True
  Start xs == Start ys = xs == ys
  Node x xs == Node y ys = x == y && xs == ys
  _ == _ = False

emptyTrie = Start []

insertTrie :: Eq a => [a] -> Trie a -> Trie a
insertTrie [] t = t
insertTrie (x:xs) Empty = Node x [insertTrie xs Empty]
insertTrie (x:xs) (Start ys) 
  | isNewBranchNeed = Start ((insertTrie (x:xs) Empty):ys)
  | otherwise = Start $ map (insertTrie (x:xs)) ys
  where isNewBranchNeed = x `notElem` map (\(Node v _) -> v) ys
insertTrie (x:xs) (Node v ys)
  | x == v && null xs = (Node v (Empty:ys))
  | x == v && not (null xs) =
    if isNewBranchNeed then 
      Node v (insertTrie xs Empty:ys)
    else
      Node v $ map (insertTrie xs) ys
  | otherwise = Node v ys
  where isNewBranchNeed = head xs `notElem` map (\(Node v _) -> v) ys

search :: Eq a => [a] -> Trie a -> Bool
search xs t = searchWithFunc xs t (Empty `elem`)

startsWith :: Eq a => [a] -> Trie a -> Bool
startsWith xs t = searchWithFunc xs t (\x -> True)

{-
  Generic function pass a function to searchAuxWithFunc,
  to distinguish the difference between search and startWith
-}
searchWithFunc :: Eq a => [a] -> Trie a -> ([Trie a] -> Bool) -> Bool
searchWithFunc [] _ _ = False
searchWithFunc xs Empty _ = False
searchWithFunc xs (Start ys) f = searchAuxWithFunc xs ys f
searchWithFunc (x:xs) (Node y ys) f = x == y && searchAuxWithFunc xs ys f

searchAuxWithFunc :: Eq a => [a] -> [Trie a] -> ([Trie a] -> Bool) -> Bool
searchAuxWithFunc _ [] _ = False
searchAuxWithFunc [] xs f = f xs
searchAuxWithFunc (x:xs) ((Node y next):ys) f = 
  if x == y then searchAuxWithFunc xs next f else searchAuxWithFunc (x:xs) ys f
searchAuxWithFunc _ _ _ = False

{-
  36. Coin Change

  You are given an integer array coins representing coins of different denominations 
  and an integer amount representing a total amount of money.

  Find the fewest number of coins that you need to make up that amount.
  1. Assume your have unlimited supply
  2. What if you have limit supply of coins?
    The implementation of this part 2 should be fairly similar to part 1.
-}

-- sorted in an ascending order
supply :: [Int] 
supply = [100,50,20,10,5,1]

type Exchange = [Int]
type FaceValue = [Int]

{-
  greed algorithm:

  choose the largest face value first, add to exchange
    if exchange = target, output exchange
    else if exchange < target, 
      add the largest value of current face value to exchange
    else if exchange > target
      remove the largest value in face values,
      add the largest value of new face values to exchange
    else if we run out of face values
      the amount can not be exchange on current supply
-}
cashier :: FaceValue -> Int -> Exchange
cashier fv t = cashierAux fv t []

cashierAux :: FaceValue -> Int -> Exchange -> Exchange
cashierAux [] _ _ = error "No exchange for this amount"
cashierAux fv t ex
  | sum ex == t = ex
  | sum ex < t = cashierAux fv t ((head fv): ex)
  | sum ex > t = cashierAux (tail fv) t ((head $ tail fv): tail ex)

{-
  37. Product of Array Except Self

  Given an array of numbers, return an array that each element is products of 
  the rest of array of numbers.

  You must write an algorithm that runs in O(n) time and without using the 
  division operation.

  [1,2,3,4,5] -> [120,60,40,30,24]
-}

-- with division operation
productArrayDivide :: [Int] -> [Int]
productArrayDivide arr = map (product `div`) arr
  where product = foldl (*) 1 arr

-- with prefix and suffix
productArrayTwoParts :: [Int] -> [Int]
productArrayTwoParts xs = zipWith (*) pref suff
  where pref = prefix xs Nothing
        suff = suffix xs

prefix :: [Int] -> Maybe Int -> [Int]
prefix [] _ = []
prefix (x:xs) Nothing = 1: (prefix xs (Just x))
prefix (x:xs) (Just n) = n: (prefix xs (Just (x*n)))

suffix :: [Int] -> [Int]
suffix arr = reverse $ prefix (reverse arr) Nothing

-- prefix and suffix with only one array instead of two arrays
productArrayOnePart :: [Int] -> [Int]
productArrayOnePart xs = reverse 
  $ suffixMultiply (reverse xs) (reverse (prefix xs Nothing)) Nothing

suffixMultiply :: [Int] -> [Int] -> Maybe Int -> [Int]
suffixMultiply [] _ _ = []
suffixMultiply _ [] _ = []
suffixMultiply (x:xs) (y:ys) acc =
  case acc of
    Nothing -> suffixMultiply (x:xs) (y:ys) (Just 1)
    Just n -> (n*y): suffixMultiply (xs) (ys) (Just (n*x))

{-
  38. Min Stack

  Design a stack that supports push, pop, top, and retrieving the minimum 
  element in constant time.

  Implement the MinStack class:
  - push: pushes the element val onto the stack.
  - pop: removes the element on the top of the stack.
  - top: gets the top element of the stack.
  - min: retrieves the minimum element in the stack.
  You must implement a solution with O(1) time complexity for each function.

  Test case
  1. push [5,3,7,1,1] sequentially into an emptyMinStack ->
     Stack (Just 1) [1,1,7,3,5] [1,1,3,5]
-}

{-
  Maybe a: store min value or Nothing
  first [a]: regular stack
  second [a]: stack keep values of used min
-}
data MinStack a = Stack (Maybe a) [a] [a] deriving (Show)

emptyMinStack = Stack Nothing [] []

push :: Ord a => a -> MinStack a -> MinStack a
push n (Stack Nothing xs ys) = Stack (Just n) (n:xs) (n:ys)
push n (Stack (Just m) xs ys)
  | n <= m = Stack (Just n) (n:xs) (n:ys)
  | otherwise = Stack (Just m) (n:xs) ys

pop :: Ord a => MinStack a -> MinStack a
pop (Stack Nothing _ _) = error "empty stack"
pop (Stack (Just m) xs ys)
  | m == head xs = 
    if null nextMins then emptyMinStack 
    else Stack (Just (head nextMins)) (tail xs) nextMins
  | otherwise = Stack (Just m) (tail xs) ys
  where nextMins = tail ys

top :: MinStack a -> a
top (Stack _ [] _) = error "empty stack"
top (Stack _ xs _) = head xs

stackMin :: MinStack a -> a
stackMin (Stack Nothing _ _) = error "empty stack"
stackMin (Stack (Just m) _ _) = m

{-
  39. Validate Binary Search Tree

  Given the root of a binary tree, determine if it is a binary search tree.

  Binary search tree (BST): is a binary tree data structure with the key of 
  each internal node being greater than all the keys in the respective node's 
  left subtree and less than the ones in its right subtree.
-}

goodbst = 
  Node' 3 (
    Node' 1 (Empty',Empty'), 
    Node' 5 (Node' 4 (Empty',Empty'), 
             Node' 6 (Empty',Empty')))

badbst =   
  Node' 3 (
    Node' 1 (Empty',Empty'), 
    Node' 4 (Node' 5 (Empty',Empty'), 
             Node' 6 (Empty',Empty')))

isBinSearchTree :: Ord a => BiTree a -> Bool
isBinSearchTree Empty' = True
isBinSearchTree (Node' n (l,r)) = 
  isBinSearchTreeAux n l (<=) && isBinSearchTreeAux n r (>)

isBinSearchTreeAux n Empty' f = True
isBinSearchTreeAux n (Node' n' (l,r)) f = 
  (f n' n) && isBinSearchTreeAux n' l (<=) && isBinSearchTreeAux n' r (>)

{-
  40. Number of Islands

  Given an m x n 2D binary grid which represents a map of '1's (land) and '0's 
  (water), return the number of islands.

  An island is surrounded by water and is formed by connecting adjacent lands 
  horizontally or vertically. You may assume all four edges of the grid are all 
  surrounded by water.
  
  Test Cases:
  numIslands grid1 -> 1
  numIslands grid2 -> 3
-}

type Grid = [[Int]] 

grid1 :: Grid
grid1 = [[1,1,1,1,0],
         [1,1,0,1,0],
         [1,1,0,0,0],
         [0,0,0,0,0]]

grid2 :: Grid
grid2 = [[1,1,0,0,0],
         [1,1,0,0,0],
         [0,0,1,0,0],
         [0,0,0,1,1]]

-- create row by column matrix
mBynMatrix :: Int -> Int -> a -> [[a]]
mBynMatrix m n v = replicate m (replicate n v)

-- all coordinates for the grid
coordsOf :: Grid -> [(Int,Int)]
coordsOf g = coordsOfAux g 0

coordsOfAux :: Grid -> Int -> [(Int,Int)]
coordsOfAux [] _ = []
coordsOfAux (x:xs) r = (map (\x -> (r,x)) [0..(length x - 1)]) ++ coordsOfAux xs (r+1)

numIslands :: Grid -> Int
numIslands g = numIslandsAux g []

-- use diffs to track which coordinates have not be visited
numIslandsAux :: Grid -> [(Int,Int)] -> Int
numIslandsAux g visited =
  case diffs of
    [] -> 0
    (r,c):_ -> 
      if g!!r!!c == 1 then
        1 + numIslandsAux g ((findAreaInColor g [(r,c)] 1 []) ++ visited)
      else
        numIslandsAux g ((findAreaInColor g [(r,c)] 0 []) ++ visited)
  where allCoords = coordsOf g
        diffs = allCoords \\ visited

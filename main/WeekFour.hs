module WeekFour where

import Data.List

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
  
  What if you have limit supply of coins?
-}

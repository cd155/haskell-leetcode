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

-- use number to represent class
type Class = Char

type Prereq = (Class, Class)

type Graph = ([Class], [Prereq])

-- visit all node in a undirected graph
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

-- detect cycle in a direct graph
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

-- visit all node in a directed graph
topoSort :: Graph -> [Class]
topoSort ([],_) = []
topoSort (v,e) = topoSortWeaver (v,e) []

topoSortWeaver :: Graph -> [Class] -> [Class]
topoSortWeaver (v,e) visited
  | null diffs = visited
  | otherwise = topoSortWeaver (v,e) newVisited
  where newVisited = topoSortAux e [head diffs] visited
        diffs = v \\ visited 

topoSortAux :: [Prereq] -> [Class] -> [Class] -> [Class]
topoSortAux _ [] visited = visited
topoSortAux ps (x:xs) visited =
  case adjacencies of
    [] -> topoSortAux ps xs (x:visited)
    _  -> topoSortAux ps (adjacencies ++ (x:xs)) (visited)
  where adjacencies = filter (\z -> z `notElem` visited && z `notElem` (x:xs)) possClasses
        possClasses = map snd (filter (\(x',_) -> x' == x) ps)


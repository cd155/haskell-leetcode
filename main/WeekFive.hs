module WeekFive where

import Data.List
import qualified Data.Map as M
import WeekOne (BiTree (Empty', Node'), findLCA)

{-
  42. Search in Rotated Sorted Array

  Given a sorted array of n integers that has been rotated with a unknown 
  pivot for one time, write code to find an element in the array. You may 
  assume that the array was originally sorted in increasing order.

  Test Cases:
  searchRotatedArr 6  [4,5,6,7,0,1,2] -> Just 2
  searchRotatedArr 0  [4,5,6,7,0,1,2] -> Just 4
  searchRotatedArr 10 [4,5,6,7,0,1,2] -> Nothing
  searchRotatedArr 6  [6,1,2]         -> Just 0
  searchRotatedArr 2  [6,1,2]         -> Just 2
-}
searchRotatedArr :: Ord a => a -> [a] -> Maybe Int
searchRotatedArr _ [] = Nothing
searchRotatedArr t xs 
  | xs !! ind == t = Just ind
  | otherwise = Nothing
  where ind = searchRotatedArrAux t xs

searchRotatedArrAux :: Ord a => a -> [a] -> Int
searchRotatedArrAux t [x] = 0
searchRotatedArrAux t xs
  | head l <= last l = -- left is in order
    if t >= head l && t <= last l then -- t within l
      searchRotatedArrAux t l
    else length l + searchRotatedArrAux t r 
  | otherwise = -- right is in order
    if t >= head r && t <= last r then -- t within r
      length l + searchRotatedArrAux t r
    else 
      searchRotatedArrAux t l
  where mid = length xs `div` 2
        (l,r) = splitAt mid xs

{-
  43. Combination Sum

  Given an array of distinct integers and a target integer, return a list 
  of all unique combinations of elements in this array where sum of the 
  chosen numbers equal to target.

  Test Cases
  combSum (7,150) [2,3,6,7] -> [[3,2,2],[7]]
  combSum (8,150) [2,3,5]   -> [[2,2,2,2],[3,3,2]]
-}

{-
  find combination with first element + second element +...

  target: the finding number
  max: max length of the combinations
  (x:xs): available elements

  (assume max length of combination >= 1)
-}
combSum :: (Int,Int) -> [Int] -> [[Int]]
combSum _ [] = []
combSum (target,max) (x:xs)
  | x == target = [[x]] ++ combSum (target,max) xs
  | otherwise = 
    combSumAux [[x]] (x:xs) (target,max) [] ++ combSum (target,max) xs

-- continue refine combinations until they are no longer valid
-- combSumAux [[2]] [2,3,5] (8,150) [] -> [[2,2,2,2],[3,3,2]]
combSumAux :: [[Int]] -> [Int] -> (Int,Int) -> [[Int]] -> [[Int]]
combSumAux [] _ _ acc = acc
combSumAux (x:xs) ys (target,max) acc = 
  combSumAux (xs ++ refineComb) ys (target,max) (readyAns++acc)
  where goodPending = filter (>= head x) ys
        allComb = (map (:x) goodPending)
        readyAns = 
          filter (\x -> sum x == target && length x <= max) allComb
        refineComb = 
          filter (\x -> sum x < target && length x <= max) allComb

{-
  44. Permutations

  Given an array nums of distinct integers, return all the possible 
  permutations. You can return the answer in any order.

  Test Cases:
  permutations [1,2,3] -> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-}

permutations :: [Int] -> [[Int]]
permutations xs = permutationsAux xs xs

{-
  collect permutations with different start elements
  first [Int]: for each permutation with different start
  second [Int]: all possible elements of combinations
-}
permutationsAux :: [Int] -> [Int] -> [[Int]]
permutationsAux [] _ = []
permutationsAux (x:xs) ys = 
  (permutationsWith [[x]] ys) ++ (permutationsAux xs ys)

{-
  all permutation with particular start
  permutationsWith [[1]] [1,2,3] -> [[3,2,1],[2,3,1]]
  permutationsWith [[2]] [1,2,3] -> [[3,1,2],[1,3,2]]
-}
permutationsWith :: [[Int]] -> [Int] -> [[Int]]
permutationsWith [] ys = [] -- not design to have []
permutationsWith (x:xs) ys
  | length x == length ys = x:xs
  | otherwise = permutationsWith (xs ++ allComb) ys
  where allComb = filter (\x -> not (null x)) 
          (map (\y -> if y `notElem` x then y:x else []) ys)

{-
  45. Merge Intervals

  Given an array of intervals, merge all overlapping intervals, and return 
  an array of the non-overlapping intervals

  Test Cases:
  mergeIntervals [(1,3),(2,6),(8,10),(10,12),(15,18)] 
    -> [(1,6),(8,12),(15,18)]
  mergeIntervals [(1,4),(4,5)] -> [(1,5)]
-}

type Interval = (Int,Int)

-- Assume the [Interval] is sorted ascending base on the start of Interval
mergeIntervals :: [Interval] -> [Interval]
mergeIntervals [] = []
mergeIntervals [x] = [x]
mergeIntervals ((x1s,x1e):(x2s,x2e):xs)
  | x2s <= x1e = mergeIntervals ((x1s,x2e):xs)
  | otherwise = (x1s,x1e): mergeIntervals ((x2s,x2e):xs)

{-
  46. Lowest Common Ancestor of a Binary Tree

  Given a binary tree, find the lowest common ancestor (LCA) (also called 
  least common ancestor)of two given nodes in the tree.

  findLCA tree46 6 4 -> 5
  findLCA tree46 7 4 -> 2
  findLCA tree46 8 2 -> 3
-}

tree46 =
  Node'
    3
    ( Node'
        5
        ( Node' 6 (Empty', Empty'),
          Node' 2 (Node' 7 (Empty', Empty'), Node' 4 (Empty', Empty'))
        ),
      Node'
        1
        (Node' 0 (Empty', Empty'), Node' 8 (Empty', Empty'))
    )

{-
  47. Time Based Key-Value Store

  Design a time-based key-value data structure that can store multiple 
  values for the same key at different time stamps and retrieve the key's 
  value at a certain timestamp.

  Implement the TimeMap:
  1. update(TimeMap dict, String key, int timestamp, any value) 
  Stores the key with the value value at the given time timestamp.
  2. lookup(TimeMap dict, String key, int timestamp) Returns a value by 
  accessing key and timestamp in TimeMap. If current timestamp does not 
  existed, return the value from the largest time stamp.

  Test Cases:
  lookupTimeMap testTimeMap "foo"  1 -> Just "bar"
  lookupTimeMap testTimeMap "foo"  5 -> Just "bar2"
  lookupTimeMap testTimeMap "abc"  2 -> Just "hey"
  lookupTimeMap testTimeMap "good" 2 -> Nothing
-}
testTimeMap = update (update (update M.empty "foo" 1 "bar") "foo" 4 "bar2") "abc" 1 "hey" 

type TimeMap a = M.Map String (M.Map Int a)

update :: TimeMap a -> String -> Int -> a -> TimeMap a
update timeMap key timestamp value 
  | key `M.member` timeMap = M.adjust (\_ -> newValForExistKey) key timeMap
  | otherwise = M.insert key newValForNonExistKey timeMap
  where Just innerDict = M.lookup key timeMap
        newValForExistKey = if timestamp `M.member` innerDict then
                              M.adjust (\_ -> value) timestamp innerDict
                            else
                              M.insert timestamp value innerDict
        newValForNonExistKey = M.insert timestamp value M.empty

lookupTimeMap :: TimeMap a -> String -> Int -> Maybe a
lookupTimeMap timeMap key timestamp
  | key `M.member` timeMap = 
      if timestamp `M.member` innerDict then
        Just (innerDict M.! timestamp)
      else
        if null allKeysFromInnerDict then 
          Nothing
        else
          Just (innerDict M.! (last allKeysFromInnerDict))
  | otherwise = Nothing
  where Just innerDict = M.lookup key timeMap
        allKeysFromInnerDict = M.keys innerDict

{-
  48. Accounts Merge

  Given a list of accounts where each element accounts[i] is a list of 
  strings, where the first element accounts[i][0] is a name, and the rest 
  of the elements are emails representing emails of the account.

  Now, we would like to merge these accounts. Two accounts definitely 
  belong to the same person if there is some common email to both accounts. 
  Note that even if two accounts have the same name, they may belong to 
  different people as people could have the same name.

  After merging the accounts, return the accounts in the following format: 
  the first element of each account is the name, and the rest of the 
  elements are emails in sorted order. The accounts themselves can be 
  returned in any order.

  Test Cases:

  merge testAccountForGroup -> [["Don","w@123.com"],
                                ["Don","b@123.com",
                                       "c@123.com",
                                       "a@123.com",
                                       "z@123.com",
                                       "d@123.com"],
                                ["Tom","tom@123.com"]]
-}

type Account = [String]

{-
  Assume A and B is the same person with some common email and B and C is the 
  same person with some common email, even A and C has not email in common, 
  by transitivity A and C are the same person.

  Thought:
  1. Group accounts base on its name.
  2. In each group, treat them as a graph (V,E)
    V: set of Node
    E: set of edge
    Create V and E
    
    Arbitrarily visit an unvisited node, and visit connected node. All the 
    connected node is a new account 
    
    visit all nodes in this graph
  3. gathering results from each group
-}

testAccountForGroup = [["Don", "z@123.com", "a@123.com", "d@123.com"],
                       ["Don", "a@123.com", "b@123.com", "d@123.com"],
                       ["Don", "b@123.com", "c@123.com"],
                       ["Don", "w@123.com"],
                       ["Tom", "tom@123.com"]]

-- using dictionary to group account base on its name
convertToDict :: [Account] -> M.Map String [Account]
convertToDict xs = convertToDictAux xs M.empty

convertToDictAux :: [Account] -> M.Map String [Account] -> M.Map String [Account]
convertToDictAux [] dict = dict
convertToDictAux (x:xs) dict
  | name `M.member` dict = convertToDictAux xs (M.insertWith (++) name [emails] dict)
  | otherwise = convertToDictAux xs (M.insert name [emails] dict)
  where name = head x
        emails = tail x

testAccountForMerge = [["z@123.com", "a@123.com", "d@123.com"],
                       ["a@123.com", "b@123.com", "d@123.com"],
                       ["b@123.com", "c@123.com"],
                       ["w@123.com"]]

type Graph a = ([a], [(a,a)])

-- create a graph with list of account
graphing :: [Account] -> Graph String
graphing xs = (nub v, nub e)
  where (v,e) = graphingAux xs ([],[])

-- G = (V,E)
graphingAux :: [Account] -> Graph String -> Graph String
graphingAux [] g = g
graphingAux (x:xs) (v,e) = graphingAux xs (v++x, e++myEdges)
  where myEdges = edges x

-- generate edges
edges :: [String] -> [(String,String)]
edges [] = []
edges [x] = []
edges (x1:x2:xs) = (x1,x2): edges (x2:xs)

-- visit all node in this graph
undirDepthFirstAllNodes :: Graph String -> [Account]
undirDepthFirstAllNodes g = undirDepthFirstAllNodesAux g []

-- using [string] to track if all node be visited
undirDepthFirstAllNodesAux :: Graph String -> [String] -> [Account]
undirDepthFirstAllNodesAux (v,e) visited
  | null diff = []
  | otherwise = newAccount: (undirDepthFirstAllNodesAux (v,e) (newAccount++visited))
  where diff = v \\ visited
        newAccount = undirDepthFirst e (head diff)

-- give a start point, find all connected node in the graph
undirDepthFirst :: [(String,String)] -> String -> Account
undirDepthFirst e startNode = undirDepthFirstAux e [startNode] []

{-
  x:xs: using stack to hold next visiting node
  visited: track visited node
-}
undirDepthFirstAux :: [(String,String)] -> [String] -> [String] -> Account
undirDepthFirstAux _ [] _ = []
undirDepthFirstAux e (x:xs) visited = x: (undirDepthFirstAux e (nonVisitedNodes++xs) (x:visited))
  where possibleEdges = filter (\(start,end) -> start == x || end == x) e
        possibleNodes = map (\(start,end) -> if start == x then end else start) possibleEdges
        nonVisitedNodes = filter (\node -> node `notElem` visited && node `notElem` (x:xs)) possibleNodes

merge :: [Account] -> [Account] 
merge accounts = mergeAux (M.keys groupDict) groupDict
  where groupDict = convertToDict accounts

-- gathering accounts for each key
mergeAux :: [String] -> M.Map String [Account] -> [Account]
mergeAux [] _ = []
mergeAux (x:xs) dict = 
  (map (\account -> x: account) newAccounts) ++ mergeAux xs dict
  where Just accounts = M.lookup x dict
        newAccounts = undirDepthFirstAllNodes (graphing accounts)

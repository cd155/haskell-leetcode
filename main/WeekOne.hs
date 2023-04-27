module WeekOne where

import Data.List
import Data.Char
import qualified Data.Map as M

{-
  1. Two Sum:

  Given an array of integers and an integer target,
  return two numbers that they add up to target.

  You can return the answer in any order.

  Test Case:
  twoSumHash [2,7,11,15] 9      -> (2,7)
  twoSumHash [3,2,4] 6          -> (2,4)
  twoSumHash [3,3] 6            -> (3,3)
  twoSumHash [5,2,3,5,1,7,1] 2  -> (1,1)
  twoPair [4,4,8,10,0] 8        -> [(0,8),(4,4),(8,0)]
-}

-- Natural number
type Nat = Int

{-
  Brutal force, O(n^2)
  In each element, check if the target exist in the rest of list.
-}
twoSumBF :: [Int] -> Int -> (Int, Int)
twoSumBF [] _ = error "No two number add up to the target" 
twoSumBF (x:xs) t
  | otherHalf `elem` xs = (x, otherHalf)
  | otherwise = twoSumBF xs t
  where otherHalf = t - x

{-
  Hash table, O(n)
  In each element, check if the target exist in the hash table.
-}
convertToDict :: [Int] -> M.Map Int Int -> M.Map Int Int
convertToDict [] dict = dict
convertToDict (x : xs) dict
  | x `M.member` dict = convertToDict xs (M.insertWith (+) x 1 dict)
  | otherwise = convertToDict xs (M.insert x 1 dict)

twoSumHash :: [Int] -> Int -> (Int, Int)
twoSumHash xs t = twoSumHashHelper xs t dict
  where dict = convertToDict xs M.empty

twoSumHashHelper :: [Int] -> Int -> M.Map Int Int -> (Int, Int)
twoSumHashHelper [] t dict = error "No two number add up to the target"
twoSumHashHelper (x:xs) t dict
  | x == otherHalf = if count >= 2 then (x, x) else twoSumHashHelper xs t dict
  | otherHalf `M.member` dict = (x,otherHalf)
  | otherwise = twoSumHashHelper xs t dict
  where otherHalf = t - x
        Just count = M.lookup x dict

-- Find all the two sum solution in the list
twoPair :: [Int] -> Int -> [(Int, Int)]
twoPair xs target = twoPairAux (M.keys myDict) myDict target
  where myDict = convertToDict xs M.empty

twoPairAux :: [Int] -> M.Map Int Int -> Int  -> [(Int, Int)]
twoPairAux [] _ _ = []
twoPairAux (x : xs) dict t
  | x == otherHalf = 
    if count >= 2 then 
      (x,otherHalf): twoPairAux xs dict t 
    else 
      twoPairAux xs dict t
  | otherHalf `M.member` dict = (x,otherHalf) : twoPairAux xs dict t
  | otherwise = twoPairAux xs dict t
  where otherHalf = t - x
        Just count = M.lookup x dict

{-
  2. Valid Parentheses:

  Given a string s containing just the characters '(', ')', '{', '}', '[' and ']',
  determine if the input string is valid.

  An input string is valid if:

  Open brackets must be closed by the same type of brackets.
  Open brackets must be closed in the correct order.
  Every close bracket has a corresponding open bracket of the same type.

  Test Cases:

  isValidParens "()"          -> True
  isValidParens "()[]{}"      -> True
  isValidParens "([{()}{}])"  -> True
  isValidParens "(]"          -> False
  isValidParens "(([][])[})"  -> False
-}

-- hash contains cancelable characters
closeParens = M.fromList [(')', '('),
                          (']', '['),
                          ('}', '{')]

isValidParens :: String -> Bool
isValidParens [] = error "empty string"
isValidParens xs = isValidParensAux (tail xs) [head xs]

isValidParensAux :: String -> String -> Bool
isValidParensAux [] stack = null stack
isValidParensAux (x:xs) stack
  | (x `M.member` closeParens) && (leftParen == head stack) = 
    isValidParensAux xs (tail stack)
  | otherwise = isValidParensAux xs (x:stack)
  where Just leftParen = M.lookup x closeParens

{-
  3. Merge Two Sorted Single Linked Lists

  You are given the heads of two sorted linked lists list1 and list2.
  Merge the two linked lists in a one sorted list. Return the head of the
  merged linked list.

  Test Cases:
  mergeLists  link1 link2 -> 
    Node 1 (Node 1 (Node 2 (Node 3 (Node 4 (Node 4 Empty)))))
  mergeLists' link1 link2 -> 
    Node 1 (Node 1 (Node 2 (Node 3 (Node 4 (Node 4 Empty)))))
  mergeLists' link3 link4 -> Empty
  mergeLists' link5 link6 -> Node 0 Empty
-}

(link1,link2) = (Node 1 (Node 2 (Node 4 Empty)), Node 1 (Node 3 (Node 4 Empty)))
(link3,link4) = (Empty, Empty)
(link5,link6) = (Empty, Node 0 Empty)

data LinkedList a = Empty | Node a (LinkedList a) deriving (Show)

-- only for practice, make LinkedList equatable
instance Eq a => Eq (LinkedList a) where
  Empty == Empty = True
  Empty == (Node _ _) = False
  (Node _ _) == Empty = False
  (Node v1 n1) == (Node v2 n2) = v1 == v2 && n1 == n2

-- take list1 as default, insert elements from list2 at a time O(n*m)
mergeLists :: Ord a => LinkedList a -> LinkedList a -> LinkedList a
mergeLists l1 Empty = l1
mergeLists l1 (Node v next) = mergeLists (insertNode l1 (Node v Empty)) next

insertNode :: Ord a => LinkedList a -> LinkedList a -> LinkedList a
insertNode l1 Empty = l1
insertNode Empty l2 = l2
insertNode (Node v1 next1) (Node v2 next2)
  | v1 < v2 = Node v1 (insertNode next1 (Node v2 next2))
  | otherwise = Node v2 (Node v1 next1)

-- Loop two lists at the same time O(n+m)
mergeLists' :: Ord a => LinkedList a -> LinkedList a -> LinkedList a
mergeLists' l1 Empty = l1
mergeLists' Empty l2 = l2
mergeLists' (Node v1 next1) (Node v2 next2)
  | v1 < v2 = Node v1 (mergeLists' next1 (Node v2 next2))
  | otherwise = Node v2 (mergeLists' (Node v1 next1) next2)

{-
  4. Best Time to Buy and Sell Stock

  You are given passed data in an array prices where prices[i] is the price
  of a given stock on the ith day. You want to maximize your profit by
  choosing a single day to buy one stock and choosing a different day
  in the future to sell that stock.

  Return the maximum profit you can achieve from this transaction.
  If you cannot achieve any profit, return 0.

  maxProfit  stocks1 -> 5
  maxProfit  stocks2 -> -1
  maxProfit' stocks2 -> 0
  maxProfit' stocks3 -> 10
-}

[stocks1,stocks2,stocks3] = [[7, 1, 5, 3, 6, 4],
                             [7, 6, 4, 3, 1],
                             [8, 7, 1, 5, 3, 6, 7, 0, 5, 10]] :: [[Int]]

-- find every pairs of the list, then find the maximum O(n^2)
maxProfit :: [Int] -> Int
maxProfit xs = maximum (map (\(buy,sell) -> sell - buy) allPairs)
  where allPairs = allPairOf xs

allPairOf :: [Int] -> [(Int, Int)]
allPairOf [] = []
allPairOf (x : xs) = x `pairOf` xs ++ allPairOf xs

pairOf :: Int -> [Int] -> [(Int, Int)]
pairOf _ [] = []
pairOf x (y:ys) = (x,y): x `pairOf` ys

-- Keep track bottom and maxProfit
maxProfit' :: [Int] -> Int
maxProfit' [] = 0
maxProfit' xs = maxProfitHelper xs (head xs) 0

maxProfitHelper :: [Int] -> Int -> Int -> Int
maxProfitHelper [] _ maxPro = maxPro
maxProfitHelper (x:xs) minPrice maxPro
  | x < minPrice = maxProfitHelper xs x maxPro
  | otherwise = maxProfitHelper xs minPrice newMax
  where newMax = max maxPro (x - minPrice)

{-
  5. Valid Palindrome

  A phrase is a palindrome if, after converting all uppercase letters
  into lowercase letters and removing all non-alphanumeric characters,
  it reads the same forward and backward. Alphanumeric characters include
  letters and numbers.

  Given a string s, return true if it is a palindrome, or false otherwise.

  Test:
  isPalindrome "A man, a plan, a canal: Panama" -> True
  isPalindrome "dogod"                          -> True
  left to right: dogod
  right to left: dogod
  isPalindrome "race a car"                     -> False
  isPalindrome " "                              -> True
-}

-- clean string
cleanStr :: String -> String
cleanStr [] = []
cleanStr (x:xs)
  | lc `elem` lAlphabet = lc: cleanStr xs
  | otherwise = cleanStr xs
  where lc = toLower x
        lAlphabet = ['a'..'z']

-- run palindrome checker
isPalindrome :: String -> Bool
isPalindrome xs
  | f == reverse s || f == reverse (tail s) = True
  | otherwise = False
  where cleanedStr = cleanStr xs
        mid = length cleanedStr `div` 2
        (f, s) = splitAt mid cleanedStr

{-
  6. Invert a binary tree

  once swap to node, children go with original, they need to swap as well
-}
data BiTree a = Empty' | Node' a (BiTree a, BiTree a) deriving (Show)

instance Eq a => Eq (BiTree a) where
  Empty' == Empty' = True
  Empty' == Node' {} = False
  Node' {} == Empty' = False
  Node' x (l1, r1) == Node' y (l2, r2) = x == y && l1 == l2 && r1 == r2

-- [1,2,3,n,n,4]
tree1 = Node' 1 
          (Node' 2 (Empty', Empty'), 
           Node' 3 (Node' 4 (Empty', Empty'), Empty'))

-- [1,3,2,n,4]
tree2 = Node' 1 
          (Node' 3 (Empty', Node' 4 (Empty', Empty')), 
           Node' 2 (Empty', Empty'))

invert :: BiTree a -> BiTree a
invert Empty' = Empty'
invert (Node' v (c1, c2)) = Node' v (invert c2, invert c1)

-- What if I just want to swap node values
swap :: BiTree a -> BiTree a
swap Empty' = Empty'
swap (Node' v (Empty', Empty')) = Node' v (Empty', Empty')
swap (Node' v (Empty', c2)) = Node' v (c2, Empty')
swap (Node' v (c1, Empty')) = Node' v (Empty', c1)
swap (Node' v (Node' cv1 (l1, r1), Node' cv2 (l2, r2))) =
  Node' v (Node' cv2 (swap r1, swap l1), Node' cv1 (swap r2, swap l2))

{-
  7. Valid Anagram

  Given two strings s and t, return true if t is an anagram of s, and false
  otherwise.

  An Anagram is a word or phrase formed by rearranging the letters of a
  different word or phrase, typically using all the original letters exactly
  once.

  anagram aka "meaningful" permutation
  All anagram are permutation

  Test case:
  isAnagram "anagram" "nagaram" -> True
  isAnagram "rat" "car"         -> False
  isAnagram "" "a"              -> False
  isAnagram "" ""               -> True
-}

-- convert string to dictionary (character: showup times)
convToDict' :: String -> M.Map Char Int
convToDict' =
  foldl
    ( \acc x ->
        if x `M.member` acc then 
          M.insertWith (+) x 1 acc -- combine new value (1) and old value
        else 
          M.insert x 1 acc
    )
    M.empty

isAnagram :: String -> String -> Bool
isAnagram s1 s2 = isAnagramHelper s1 (convToDict' s2)

isAnagramHelper :: String -> M.Map Char Int -> Bool
isAnagramHelper [] dict = foldl (\acc x -> acc && x == 0) True (M.elems dict)
isAnagramHelper (x:xs) dict
  | x `M.member` dict = isAnagramHelper xs newDict
  | otherwise = False
  where newDict = M.insertWith (+) x (-1) dict

{-
  8. Binary Search

  Given an array of integers which is sorted in ascending order,
  and an integer target, write a function to search target in the array.
  If target exists, then return its index. Otherwise, return None.

  You must write an algorithm with O(log n) runtime complexity.

  Test case:
  findInt [-1,0,3,5,9,12] 9 -> Just 4
  findInt [-1,0,3,5,9,12] 2 -> Nothing
-}

findInt :: [Int] -> Int -> Maybe Int
findInt [] _ = Nothing
findInt xs t = if xs !! i == t then Just i else Nothing
  where i = findIntHelper xs t

findIntHelper :: [Int] -> Int -> Int
findIntHelper [] _ = 0
findIntHelper [x] _ = 0
findIntHelper xs t
  | t < xs !! mid = findIntHelper left t -- find t in left part
  | otherwise = mid + findIntHelper right t -- find t in right part
  where mid = length xs `div` 2
        (left, right) = splitAt mid xs

{-
  9. Flood Fill

  An image is represented by an m x n integer grid image where image[i][j]
  represents the pixel value of the image. You are also given three
  integers  sr, sc, and color. You should perform a flood fill on the
  image starting from the pixel image[sr][sc].

  To perform a flood fill, consider the starting pixel, plus any pixels
  connected 4-directionally to the starting pixel of the same color as
  the starting pixel, plus any pixels connected 4-directionally to those
  pixels (also with the same color), and so on. Replace the color of all
  of the aforementioned pixels with color.

  Return the modified image after performing the flood fill.

  Test case:
  fillUp testImage1 (0,0) 3 ->  [[3,3,3],
                                 [3,2,3],
                                 [2,1,3]]
  fillUp testImage2 (1,1) 3 ->  [[1,1,1],
                                 [1,3,3],
                                 [1,3,3]]
  fillUp testImage2 (0,1) 3 ->  [[3,3,3],
                                 [3,2,2],
                                 [3,2,2]]
-}
testImage1 :: Image
testImage1 =
  [ [1, 1, 1],
    [1, 2, 1],
    [2, 1, 3]
  ]

testImage2 :: Image
testImage2 =
  [ [1, 1, 1],
    [1, 2, 2],
    [1, 2, 2]
  ]

type Image = [[Nat]]

type Pixel = (Nat, Nat)

type Color = Int -- integer represent color

fillUp :: Image -> Pixel -> Color -> Image
fillUp [] _ _ = []
fillUp [[]] _ _ = [[]]
fillUp img (r, c) color = foldl (\acc x -> paint acc x color) img paintList
  where paintList = findAreaInColor img [(r, c)] (img !! r !! c) []

paint :: [[a]] -> (Int, Int) -> a -> [[a]]
paint img (c, w) color = tops ++ [newLine] ++ tail bots
  where (tops, bots) = splitAt c img
        (left, right) = splitAt w $ head bots
        newLine = left ++ [color] ++ tail right

{-
  Find the list of pixels where equal to the color start with the pixel

  ((ph,pw):ps) is a stack of potential pixels need to check
  visited         is tracking which position be visited
-}
findAreaInColor :: Image -> [Pixel] -> Color -> [Pixel] -> [Pixel]
findAreaInColor _ [] _ _ = []
findAreaInColor img ((r, c) : ps) color visited
  -- skip outbound pixel
  | r >= (length img) || c >= (length $ head img) || r < 0 || c < 0 = 
    findAreaInColor img ps color visited
  -- skip repeated pixel
  | (r, c) `elem` visited = findAreaInColor img ps color visited
  | img !! r !! c == color = 
    (r, c) : findAreaInColor img newPs color ((r, c) : visited)
  | otherwise = findAreaInColor img ps color visited
  where newPs = (r -1, c): (r + 1, c): (r, c -1): (r, c + 1): ps

{-
  10. Lowest Common Ancestor of a Binary Search Tree

  Given a binary search tree (BST), find the lowest common ancestor (LCA)
  (also called least common ancestor) node of two given nodes in the BST.

  The LCA of v and w in T is the shared ancestor of v and w that is located
  farthest from the root.

  Test cases:
  findLCA tree3 2 8 => 6
  findLCA tree3 2 4 => 2
  findLCA (Node' 2 (Empty', Node' 1 (Empty', Empty'))) 2 1 -> 2
-}

tree3 =
  Node'
    6
    ( Node'
        2
        ( Node' 0 (Empty', Empty'),
          Node' 4 (Node' 3 (Empty', Empty'), Node' 5 (Empty', Empty'))
        ),
      Node'
        8
        (Node' 7 (Empty', Empty'), Node' 9 (Empty', Empty'))
    )

-- left, mid, right
inOrder :: BiTree a -> [a]
inOrder Empty' = []
inOrder (Node' a (left, right)) = inOrder left ++ [a] ++ inOrder right

-- mid, left, right, Depth search
preOrder :: BiTree a -> [a]
preOrder Empty' = []
preOrder (Node' a (left, right)) = [a] ++ preOrder left ++ preOrder right

-- left, right, mid
postOrder :: BiTree a -> [a]
postOrder Empty' = []
postOrder (Node' a (left, right)) = postOrder left ++ postOrder right ++ [a]

inOrderDepth :: BiTree a -> [Nat]
inOrderDepth xs = inOrderDepthHelper xs 0

inOrderDepthHelper :: BiTree a -> Nat -> [Nat]
inOrderDepthHelper Empty' d = []
inOrderDepthHelper (Node' a (left, right)) d =
  inOrderDepthHelper left (d + 1) ++ [d] ++ inOrderDepthHelper right (d + 1)

index :: Eq a => [a] -> a -> Nat
index [] _ = error "Element not existed"
index xs t = if xs !! i == t then i else error "Element not existed"
  where i = indexHelper xs t

indexHelper :: Eq a => [a] -> a -> Nat
indexHelper [] _ = 0
indexHelper (x : xs) t
  | x == t = 0
  | otherwise = 1 + indexHelper xs t

{-
  find the the lowest common ancestor(LCA) via range minimum query (RMQ)

  1. have a in-order list and a in-order depth list
  2. find the range in in-order list, perform RMQ in in-order depth list
  3. find the minimum depth in in-order depth list
  4. find related node associated with the min depth in in-order list
-}
findLCA :: Eq a => BiTree a -> a -> a -> a
findLCA root n1 n2 = inOrderList !! (start + (index subDepthList minDepth))
  where
    inOrderList = inOrder root
    inOrderDepthList = inOrderDepth root
    indexn1 = index inOrderList n1
    indexn2 = index inOrderList n2
    (start, end) = (min indexn1 indexn2, max indexn1 indexn2)
    subDepthList = take (end - start + 1) (drop start inOrderDepthList)
    minDepth = minimum subDepthList

{-
  11. Balanced Binary Tree

  Given a binary tree, determine if it is balanced

  Balanced Tree:
  the height of left and right subtrees of every node differ <= 1

  Test cases:
  isBalanced'' tree1 => True
  isBalanced'' tree3 => True
  isBalanced'' tree4 => False
-}
tree4 = Node' 1 (Empty', Node' 2 (Empty', Node' 3 (Empty', Node' 4 (Empty', Empty'))))

-- odd version
isBalanced :: BiTree a -> Bool
isBalanced tr = foldl (\acc (f, s) -> acc && (abs (f - s) <= 1)) True diffs
  where
    diffs = permuOfList (leavesDepthHelper tr 0)

-- O(log(n)^2)
leavesDepthHelper :: BiTree a -> Nat -> [Nat]
leavesDepthHelper Empty' d = []
leavesDepthHelper (Node' a (Empty', r)) d = [d] ++ leavesDepthHelper r (d + 1)
leavesDepthHelper (Node' a (l, Empty')) d = leavesDepthHelper l (d + 1) ++ [d]
leavesDepthHelper (Node' a (l, r)) d =
  leavesDepthHelper l (d + 1) ++ leavesDepthHelper r (d + 1)

permuOfList :: [a] -> [(a, a)]
permuOfList [] = []
permuOfList (x : xs) = x `permuOf` xs ++ permuOfList xs

permuOf :: a -> [a] -> [(a, a)]
permuOf _ [] = []
permuOf f (x : xs) = (f, x) : permuOf f xs

isBalanced' :: BiTree a -> Bool
isBalanced' Empty' = True
isBalanced' (Node' a (l, r)) = abs (height l - height r) <= 1 && isBalanced' l && isBalanced' r

height :: BiTree a -> Int
height Empty' = 0
height (Node' _ (l, r)) = 1 + max (height l) (height r)

isBalanced'' :: BiTree a -> Bool
isBalanced'' tr
  | mb == Nothing = False
  | otherwise = True
  where
    mb = isBalancedWithHeight' tr

-- preferred version
isBalancedWithHeight' :: BiTree a -> Maybe Int
isBalancedWithHeight' Empty' = Just 0
isBalancedWithHeight' (Node' _ (l, r)) =
  case (lh, rh) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just lh', Just rh') ->
      if abs (lh' - rh') <= 1
        then Just (max lh' rh' + 1)
        else Nothing
  where
    (lh, rh) = (isBalancedWithHeight' l, isBalancedWithHeight' r)

-- Monad version
isBalancedWithHeight :: BiTree a -> Maybe Int
isBalancedWithHeight Empty' = Just 0
isBalancedWithHeight (Node' _ (l, r)) = do
  lh <- isBalancedWithHeight l
  rh <- isBalancedWithHeight r
  if abs (lh - rh) <= 1
    then Just $ max lh rh + 1
    else Nothing

{-
  12. Linked List Cycle

  Given head, the head of a linked list, determine if the linked list has a
  cycle in it. There is a cycle in a linked list if there is some node in
  the list that can be reached again.

  Note: There is no pointer in functional programming language.

  Return true if there is a cycle in the linked list. Otherwise, return false.
-}
-- how to used lazy evaluation to make test case (whereCycled cycledCase) works?
cycledCase = createCycledList (Node 1 (Node 2 (Node 4 Empty)))

createCycledList :: LinkedList a -> LinkedList a
createCycledList xs =
  case xs of
    Empty -> createCycledList xs
    Node a next -> Node a (createCycledList next)

whereCycled :: Eq a => LinkedList a -> Maybe Int
whereCycled lst = whereCycledHelper lst []

whereCycledHelper :: Eq a => LinkedList a -> [a] -> Maybe Int
whereCycledHelper Empty _ = Nothing
whereCycledHelper (Node v next) visited
  | v `elem` visited = Just (index visited v)
  | otherwise = whereCycledHelper next (visited ++ [v])

{-
  13. Implement Queue using Stacks

  Only use stack methods to implement a queue
-}
{-
  only used push and pop (prepend)

  s1: e1, e2, e3
  s2: e3, e2, e1
  s2: e4, e3, e2, e1
  s1: e1, e2, e3, e4
-}
enQue :: a -> [a] -> [a]
enQue e s = reverse (e : (reverse s))
  where reverse = foldl (\acc x -> x : acc) []

deQue :: [a] -> [a]
deQue [] = []
deQue (_ : xs) = xs

peek :: [a] -> Maybe a
peek [] = Nothing
peek (x:_) = Just x

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

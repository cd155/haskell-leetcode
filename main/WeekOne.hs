module WeekOne where
import qualified Data.Map as M
import Data.Char

{-
    1. Two Sum:

    Given an array of integers and an integer target, 
    return indexes of the two numbers that they add up to target.

    You may assume that each input would have exactly one solution, 
    and you may not use the same element twice.

    You can return the answer in any order.
-}

-- Natural number
type Nat = Int

{-
    Brutal force, O(n^2)

    In each element, check if the tar exist in the rest of list.
-}
twoSumBF :: [Integer] -> Integer -> (Nat, Nat)
twoSumBF xs tar = twoSumBFHelper xs tar 0

twoSumBFHelper :: [Integer] -> Integer -> Nat -> (Nat, Nat)
twoSumBFHelper [] _ _ = error "No two number add up to the target"
twoSumBFHelper (x:xs) tar i
    | isYExist xs y = (i, i + yIndex xs y + 1)
    | otherwise = twoSumBFHelper xs tar (i+1)
    where y = tar - x

isYExist :: [Integer] -> Integer -> Bool
isYExist xs y = y `elem` xs

-- calculate the index of Y
yIndex :: [Integer] -> Integer -> Nat
yIndex xs y = yIndexHelper xs y 0

yIndexHelper :: [Integer] -> Integer -> Nat -> Nat
yIndexHelper [] _ _ = error "No y find, check isYExist"
yIndexHelper (x:xs) y i
    | x == y = i
    | otherwise = yIndexHelper xs y (i+1)

{-
    Hash table, O(n)

    In each element, check if the tar exist in the hash table.
-}
twoSumHash :: [Integer] -> Integer -> (Nat, Nat)
twoSumHash xs tar = twoSumHashHelper xs tar dict
    where dict = convToDict xs

twoSumHashHelper :: [Integer] -> Integer -> M.Map Integer [Nat] -> (Nat, Nat)
twoSumHashHelper [] tar dict = error "No two number add up to the target"
twoSumHashHelper (x:xs) tar dict 
    | x == y && length indX > 1 = (indX!!1, indX!!0) -- format (smaller #, larger #)
    | x == y && length indX <= 1 = twoSumHashHelper xs tar dict
    | y `M.member` dict = (head indX, head indY)
    | otherwise = twoSumHashHelper xs tar dict
    where y = tar - x
          Just indX = M.lookup x dict
          Just indY = M.lookup y dict

-- convert list to a hash table with indexes as values
convToDict :: [Integer] -> M.Map Integer [Nat]
convToDict xs = convToDictHelper xs M.empty 0

convToDictHelper :: [Integer] -> M.Map Integer [Nat] -> Nat -> M.Map Integer [Nat]
convToDictHelper [] dict _ = dict
convToDictHelper (x:xs) dict i
    | x `M.member` dict = convToDictHelper xs (M.insertWith (++) x [i] dict) (i+1)
    | otherwise = convToDictHelper xs (M.insert x [i] dict) (i+1)  

{-
    2. Valid Parentheses:

    Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', 
    determine if the input string is valid.
    
    An input string is valid if:

    Open brackets must be closed by the same type of brackets.
    Open brackets must be closed in the correct order.
    Every close bracket has a corresponding open bracket of the same type.
-}

-- hash contains cancelable characters
parenDict = M.fromList [
    (')', '('), 
    (']', '['),
    ('}', '{')]
  
isValidParen :: String -> Bool
isValidParen [] = error "empty string"
isValidParen xs = 
    let 
        isValidParenHelper :: String -> String -> Bool
        isValidParenHelper [] s = null s
        isValidParenHelper (y:ys) s
            | null s = isValidParenHelper ys (y:s)
            | (y `M.member` parenDict) && (lParen == head s) = 
                isValidParenHelper ys (tail s)
            | otherwise = isValidParenHelper ys (y:s)
            where Just lParen = M.lookup y parenDict
    in 
        isValidParenHelper xs []

{-
    3. Merge Two Sorted Single Linked Lists

    You are given the heads of two sorted linked lists list1 and list2.
    Merge the two linked lists in a one sorted list. Return the head of the 
    merged linked list.
-}

data LinkedList a = Empty | Node a (LinkedList a) deriving Show

instance Eq a => Eq (LinkedList a) where
    Empty == Empty = True
    Empty == (Node _ _) = False
    (Node _ _) == Empty = False
    (Node v1 n1) == (Node v2 n2) = v1 == v2 && n1 == n2

-- take list1 as default, insert elements from list2 O(n*m)
mergeLists :: Ord a => LinkedList a -> LinkedList a -> LinkedList a
mergeLists l1 Empty = l1
mergeLists l1 (Node v next) = 
    let 
        -- insert a single node in list1 (next2 is always Empty)
        insertNode :: Ord a => LinkedList a -> LinkedList a -> LinkedList a
        insertNode l1 Empty = l1
        insertNode Empty l2 = l2
        insertNode (Node v1 next1) (Node v2 next2)
            | v1 < v2 = Node v1 (insertNode next1 (Node v2 next2))
            | otherwise = Node v2 (Node v1 next1)
    in
        mergeLists (insertNode l1 (Node v Empty)) next

-- -- this is case version
-- mergeLists l1 l2 = 
--     case (l1, l2) of
--          (l1, Empty) -> l1
--          (Empty, l2) -> l2
--          (l1, Node v next) -> 
--             let 
--                 -- insert a single node in list1 (next2 is always Empty)
--                 insertNode :: Ord a => LinkedList a -> LinkedList a -> LinkedList a
--                 insertNode l1 Empty = l1
--                 insertNode Empty l2 = l2
--                 insertNode (Node v1 next1) (Node v2 next2)
--                     | v1 < v2 = Node v1 (insertNode next1 (Node v2 next2))
--                     | otherwise = Node v2 (Node v1 next1)
--             in
--                 mergeLists (insertNode l1 (Node v Empty)) next 

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
-}

-- find every pairs of the list, then find the maximum O(n^2)
maxProfit :: [Int] -> Int
maxProfit xs = if maxPro < 0 then 0 else maxPro
    where allPairs = allPairOf xs
          maxPro = maximum (map (\x -> snd x - fst x) allPairs)

allPairOf :: [Int] -> [(Int, Int)]
allPairOf [] = []
allPairOf (x:xs) = 
    let 
        pairOf :: Int -> [Int] -> [(Int, Int)]
        pairOf _ [] = []
        pairOf x (y:ys) = (x, y): x `pairOf` ys
    in
        x `pairOf` xs ++ allPairOf xs

-- Keep track bottom and maxProfit
maxProfit' :: [Int] -> Int
maxProfit' [] = 0
maxProfit' xs = maxProfitHelper xs (head xs) 0

maxProfitHelper :: [Int] -> Int -> Int -> Int
maxProfitHelper [] _ maxPro = maxPro
maxProfitHelper (x:xs) minPrice maxPro
    | x < minPrice = maxProfitHelper xs x maxPro
    | otherwise = maxProfitHelper xs minPrice newMax
    where newMax = max maxPro (x-minPrice)

{-
    5. Valid Palindrome
    
    A phrase is a palindrome if, after converting all uppercase letters 
    into lowercase letters and removing all non-alphanumeric characters, 
    it reads the same forward and backward. Alphanumeric characters include 
    letters and numbers.

    Given a string s, return true if it is a palindrome, or false otherwise.
    
    Test:
        isPalindrome "A man, a plan, a canal: Panama" -> True
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
    | reverse f == s || reverse f == tail s = True
    | otherwise = False
    where cleanedData = cleanStr xs
          mid = length cleanedData `div` 2
          (f,s) = splitAt mid cleanedData

{-
    6. Invert a binary tree

    once swap to node, children go with original, they need to swap as well
-}
data BiTree a = Empty' | Node' a (BiTree a, BiTree a) deriving Show

instance Eq a => Eq (BiTree a ) where
    Empty' == Empty' = True
    Empty' == Node' {} = False
    Node' {} == Empty' = False
    Node' x (l1,r1) == Node' y (l2,r2) = x == y && l1 == l2 && r1 == r2 

-- [1,2,3,n,n,4]
tree1 = Node' 1 (Node' 2 (Empty', Empty'), Node' 3 (Node' 4 (Empty', Empty'), Empty'))
-- [1,3,2,n,4]
tree2 = Node' 1 (Node' 3 (Empty', Node' 4 (Empty', Empty')), Node' 2 (Empty', Empty'))

invert :: BiTree a -> BiTree a
invert Empty' = Empty'
invert (Node' v (c1,c2)) = Node' v (invert c2, invert c1)

-- What if I just want to swap node values
swap :: BiTree a -> BiTree a
swap Empty' = Empty'
swap (Node' v (Empty',Empty')) = Node' v (Empty',Empty')
swap (Node' v (Empty',c2)) = Node' v (c2, Empty')
swap (Node' v (c1, Empty')) = Node' v (Empty', c1)
swap (Node' v (Node' cv1 (l1,r1), Node' cv2 (l2,r2)))
    = Node' v (Node' cv2 (swap r1, swap l1), Node' cv1 (swap r2, swap l2))

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
    isAnagram """"                -> True
-}

-- convert string to dictionary (character: showup times)
convToDict' :: String -> M.Map Char Integer
convToDict' xs = foldl (\acc x -> 
                        if x `M.member` acc 
                        then M.insertWith (+) x 1 acc -- combine new value (1) and old value
                        else M.insert x 1 acc
                        ) M.empty xs

isAnagram :: String -> String -> Bool
isAnagram s1 s2 = isAnagramHelper s1 (convToDict' s2)

isAnagramHelper :: String -> M.Map Char Integer -> Bool
isAnagramHelper [] d2 = foldl (\acc x -> acc && x == 0) True vs
    where vs = M.elems d2
isAnagramHelper (x:xs) d2
    | x `M.member` d2 = isAnagramHelper xs newDict
    | otherwise = False
    where newDict =  M.insertWith (+) x (-1) d2

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
findInt xs t = if xs!!i == t then Just i else Nothing
    where i = findIntHelper xs t

findIntHelper :: [Int] -> Int -> Int
findIntHelper [] _ = 0 
findIntHelper [x] _ = 0
findIntHelper xs t
    | t < xs!!mid = findIntHelper left t -- find t in left part
    | t == xs!!mid = mid
    | otherwise = mid + findIntHelper right t -- find t in right part
    where mid = length xs `div` 2
          (left,right) = splitAt mid xs

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
    fillUp testImage1 (0,0) 3 [[3,3,3],
                               [3,2,3],
                               [2,1,3]]
    fillUp testImage2 (1,1) 3 [[1,1,1],
                               [1,3,3],
                               [1,3,3]]
    fillUp testImage2 (0,1) 3 [[3,3,3],
                               [3,2,2],
                               [3,2,2]]
-}
testImage1 :: Image
testImage1 = [
    [1,1,1],
    [1,2,1],
    [2,1,3]]

testImage2 :: Image
testImage2 = [
    [1,1,1],
    [1,2,2],
    [1,2,2]]

type Image = [[Nat]]
type Pixel = (Nat, Nat)
type Color = Int -- integer represent color

fillUp :: Image -> Pixel -> Color -> Image
fillUp [] _ _ = []
fillUp [[]] _ _ = [[]]
fillUp img (ph, pw) c = foldl (\acc x -> paint acc x c) img paintList
    where paintList = findAreaInColor img [(ph, pw)] (img!!ph!!pw) []

paint :: Image -> Pixel -> Color -> Image
paint img (ph, pw) c = tops ++ [newLine] ++ tail bots
    where (tops, bots) = splitAt ph img
          (l, r) = splitAt pw $ head bots
          newLine = l ++ [c] ++ tail r

{-
    Find the list of pixels where equal to the color start with the pixel

    ((ph,pw):ps) is a stack of potential pixels need to check
    dict         is the dictionary to skip checked pixel
-}
findAreaInColor :: Image -> [Pixel] -> Color -> [Pixel]-> [Pixel]
findAreaInColor _ [] _ _ = []
findAreaInColor img ((ph,pw):ps) c dict
    -- skip outbound pixel
    | ph >= h || pw >= w || ph < 0 || pw < 0= findAreaInColor img ps c dict
    -- skip repeated pixel
    | (ph,pw) `elem` dict = findAreaInColor img ps c dict
    | img!!ph!!pw == c = (ph,pw): findAreaInColor img newPs c newDict
    | otherwise = findAreaInColor img ps c dict
    where h = length img
          w = length $ head img
          newDict = (ph,pw):dict
          newPs = ps ++ [(ph-1, pw),(ph+1, pw),(ph, pw-1),(ph, pw+1)]

{-
    10. Lowest Common Ancestor of a Binary Search Tree

    Given a binary search tree (BST), find the lowest common ancestor (LCA)
    (also called least common ancestor) node of two given nodes in the BST.

    The LCA of v and w in T is the shared ancestor of v and w that is located 
    farthest from the root. 

    Test cases:
    findLCA tree3 2 8 => 6
    findLCA tree3 2 4 => 2
    findLCA (Node' 2 (Empty', Node' 1 (Empty', Empty'))) 2 1 => 2
-}

tree3 = Node' 6 
    (Node' 2 
        (Node' 0 (Empty', Empty'), 
         Node' 4 (Node' 3 (Empty', Empty'), Node' 5 (Empty', Empty'))), 
     Node' 8 
        (Node' 7 (Empty', Empty'), Node' 9 (Empty', Empty')))

-- left, mid, right
preOrder :: BiTree a -> [a]
preOrder Empty' = []
preOrder (Node' a (left, right)) = preOrder left ++ [a] ++ preOrder right

-- mid, left, right
inOrder :: BiTree a -> [a]
inOrder Empty' = []
inOrder (Node' a (left, right)) = [a] ++ inOrder left ++ inOrder right

-- left, right, mid
postOrder :: BiTree a -> [a]
postOrder Empty' = []
postOrder (Node' a (left, right)) = postOrder left ++ postOrder right ++ [a] 

preOrderDept :: BiTree a -> [Nat]
preOrderDept xs = preOrderDepthHelper xs 0

preOrderDepthHelper :: BiTree a -> Nat -> [Nat]
preOrderDepthHelper Empty' d = []
preOrderDepthHelper (Node' a (left, right)) d = 
    preOrderDepthHelper left (d+1) ++ [d] ++ preOrderDepthHelper right (d+1)

index :: Eq a => [a] -> a -> Nat
index [] _ = error "Element not existed"
index xs t = if xs!!i == t then i else error "Element not existed"
    where i = (indexHelper xs t) -1

indexHelper :: Eq a => [a] -> a -> Nat
indexHelper [] _ = 0
indexHelper (x:xs) t 
    | x == t = 1 
    | otherwise = 1 + indexHelper xs t

{-
    find the the lowest common ancestor(LCA) via range minimum query (RMQ)

    1. have a pre-order list and a pre-order depth list
    2. find the range we want do RMQ
    3. find the minimum depth
    4. find related node associated with the min depth
-}
findLCA :: Eq a => BiTree a -> a -> a -> a
findLCA root n1 n2 = preOrderList !! (start + (index tailoredDepthList minDepth))
    where preOrderList = preOrder root
          preOrderDepthList = preOrderDept root
          (start, end) = 
            (min (index preOrderList n1) (index preOrderList n2), 
             max (index preOrderList n1) (index preOrderList n2))
          tailoredDepthList = take (end-start+1) (drop start preOrderDepthList)
          minDepth = minimum tailoredDepthList 

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
isBalanced :: Eq a => BiTree a -> Bool
isBalanced tr = foldl (\acc (f, s) -> acc && (abs(f-s) <= 1)) True diffs
    where diffs = permuOfList (leavesDepthHelper tr 0)

-- O(log(n)^2)
leavesDepthHelper :: Eq a => BiTree a -> Nat -> [Nat]
leavesDepthHelper Empty' d = []
leavesDepthHelper (Node' a (l, r)) d 
    | l == Empty' || r == Empty' = 
        leavesDepthHelper l (d+1) ++ [d] ++ leavesDepthHelper r (d+1)
    | otherwise = leavesDepthHelper l (d+1) ++ leavesDepthHelper r (d+1)

permuOfList :: [a] -> [(a,a)]
permuOfList [] = []
permuOfList (x:xs) = x `permuOf` xs ++ permuOfList xs

permuOf:: a -> [a] -> [(a,a)]
permuOf _ [] = []
permuOf f (x:xs) = (f,x): permuOf f xs

isBalanced' :: BiTree a -> Bool
isBalanced' Empty' = True
isBalanced' (Node' a (l,r))= abs (height l - height r) <= 1 && isBalanced' l && isBalanced' r

height :: BiTree a -> Int
height Empty' = 0
height (Node' _ (l, r)) = 1 + max (height l) (height r)

isBalanced'' :: BiTree a -> Bool
isBalanced'' tr
    | mb == Nothing = False
    | otherwise = True 
    where mb = isBalancedWithHeight' tr

-- preferred version
isBalancedWithHeight' :: BiTree a -> Maybe Int
isBalancedWithHeight' Empty' = Just 0
isBalancedWithHeight' (Node' _ (l,r)) =
    case (lh,rh) of
        (Nothing,_) -> Nothing
        (_,Nothing) -> Nothing
        (Just lh', Just rh') -> if abs(lh' - rh') <= 1 
                                then Just (max lh' rh' + 1)
                                else Nothing
    where (lh,rh) = (isBalancedWithHeight' l, isBalancedWithHeight' r)

-- Monad version
isBalancedWithHeight :: BiTree a -> Maybe Int
isBalancedWithHeight Empty' = Just 0
isBalancedWithHeight (Node' _ (l,r)) = do
  lh <- isBalancedWithHeight l
  rh <- isBalancedWithHeight r
  if abs (lh-rh) <= 1
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
    | otherwise = whereCycledHelper next (visited++[v])

{-
    13. Implement Queue using Stacks

    Only use stack methods to implement a queue
-}
{-
    only used push and pop
    
    s1: e1, e2, e3
    s2: e3, e2, e1
    s2: e4, e3, e2, e1
    s1: e1, e2, e3, e4
-} 
enQue :: a -> [a] -> [a]
enQue e s1 = foldl (\acc' x' -> x':acc') [] (e:s2)
    where s2 = foldl (\acc x -> x:acc) [] s1

deQue :: [a] -> [a]
deQue [] = []
deQue (_:xs) = xs

peck :: [a] -> Maybe a
peek [] = Nothing
peck (x:_) = Just x

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (_:_) = False

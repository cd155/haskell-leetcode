module WeekOne where
import qualified Data.Map as M
import Data.Char

{-
    Two Sum:

    Given an array of integers and an integer target, 
    return indexes of the two numbers that they add up to target.

    You may assume that each input would have exactly one solution, 
    and you may not use the same element twice.

    You can return the answer in any order.
-}

-- Natural number
type Nat = Integer

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
    Valid Parentheses:

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
        isValidParenHelper [] s = if null s then True else False
        isValidParenHelper (y:ys) s
            | null s = isValidParenHelper ys (y:s)
            | (y `M.member` parenDict) && (lParen == head s) = 
                isValidParenHelper ys (tail s)
            | otherwise = isValidParenHelper ys (y:s)
            where Just lParen = M.lookup y parenDict
    in 
        isValidParenHelper xs []

{-
    Merge Two Sorted Single Linked Lists

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
    Best Time to Buy and Sell Stock

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
    Invert a binary tree

    once swap to node, children go with original, they need to swap as well
-}
data BiTree a = Empty' | Node' a (BiTree a, BiTree a) deriving Show

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

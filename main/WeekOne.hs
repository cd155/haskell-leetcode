module WeekOne where
import qualified Data.Map as M

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
            | (not $ null s) && (y `M.member` parenDict) && (lParen == head s) = 
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

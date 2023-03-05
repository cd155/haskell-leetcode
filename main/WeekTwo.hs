module WeekTwo where

import qualified Data.Map as M
import Data.List
import Data.Char
import WeekOne

{-
    14. First Bad Version

    You are a product manager and currently leading a team to develop a new 
    product. Unfortunately, the latest version of your product fails the 
    quality check. Since each version is developed based on the previous 
    version, all the versions after a bad version are also bad.

    Suppose you have n versions [1, 2, ..., n] and you want to find out 
    the first bad one, which causes all the following ones to be bad.

    You are given an API bool isBadVersion(version) which returns whether 
    version is bad. Implement a function to find the first bad version. 
    You should minimize the number of calls to the API.
-}

testVersions = [False, False, False, False, True, True]

isBadVersion :: Int -> Bool
isBadVersion n 
    | null testVersions = error "Empty List"
    | otherwise = testVersions!!n

badVersion :: Maybe Int
badVersion = if isBadVersion res then Just res else Nothing
    where res = badVersionHelper (0, length testVersions -1)

badVersionHelper :: (Int, Int) -> Int
badVersionHelper (start,end)
    | start == end = start
    | isBadVersion (start+mid) = badVersionHelper (start,start+mid)
    | otherwise = badVersionHelper (start+mid+1, end)
    where mid = (end-start) `div` 2

{-
    15. Ransom Note

    Given two strings ransomNote and magazine, return true if ransomNote can 
    be constructed by using the letters from magazine and false otherwise.

    Each letter in magazine can only be used once in ransomNote.

    Test Cases:
    "a"  `isConstFrom` "b"   -> False
    "aa" `isConstFrom` "ab"  -> False
    "aa" `isConstFrom` "aab" -> True
-}

isConstFrom :: String -> String -> Bool
isConstFrom note mag = isConstFromAux note dict 
    where dict = increaseDict mag

isConstFromAux :: String -> M.Map Char Int -> Bool
isConstFromAux [] _ = True
isConstFromAux (x:xs) dict
    | x `M.member` dict && v >= 1 = True && isConstFromAux xs (M.insertWith (+) x (-1) dict)
    | otherwise = False
        where Just v = M.lookup x dict

increaseDict :: String -> M.Map Char Int
increaseDict str = increaseDictAux str M.empty

increaseDictAux :: String -> M.Map Char Int -> M.Map Char Int
increaseDictAux [] dict = dict
increaseDictAux (x:xs) dict
    | x `M.member` dict = increaseDictAux xs (M.insertWith (+) x 1 dict)
    | otherwise = increaseDictAux xs (M.insert x 1 dict)

{-
    16. Climbing Stairs

    You are climbing a staircase. It takes n steps to reach the top.

    Each time you can either climb 1 or 2 steps. In how many distinct ways can 
    you climb to the top?

    Test Cases:
    climb  2 -> 2 
    climb' 2 -> 2 
    climb  5 -> 8 
    climb' 5 -> 8 
-}
-- the same question with fibonacci number problem O(n^2)
climb :: Int -> Int
climb n
    | n == 0 = 0
    | otherwise = climbAux n

climbAux :: Int -> Int
climbAux n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = climbAux (n-2) + climbAux (n-1)

-- memorization version O(n)
climb' :: Int -> Int
climb' n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = head climbMemo
    where climbMemo = climb'Aux (2, n) [2,1]

climb'Aux :: (Int, Int) -> [Int] -> [Int]
climb'Aux (i, end) memo
    | i >= end = memo
    | otherwise = climb'Aux (i+1,end) newMemo
        where newMemo = (head memo + head (tail memo)): memo

{-
    17. Longest Palindrome

    Given a string s which consists of lowercase or uppercase letters, return 
    the length of the longest palindrome that can be built with those letters.

    Letters are case sensitive, for example, "Aa" is not considered a 
    palindrome here.

    Test Cases:
    longestPalindrome "abccccdd" -> 7
    longestPalindrome "a" -> 1
-}
longestPalindrome :: String -> Int
longestPalindrome s1 
    | isExistOdd allVals = 2 * numOfPair + 1
    | otherwise = 2 * numOfPair
    where allVals = M.elems $ increaseDict s1
          numOfPair = foldl (\acc x -> acc + x `div` 2) 0 allVals

isExistOdd :: [Int] -> Bool
isExistOdd [] = False
isExistOdd (x:xs)
    | odd x = True
    | otherwise = isExistOdd xs

{-
    18. Reverse Linked List
    
    Given the head of a singly linked list, reverse the list, and return the 
    reversed list.

    Test Cases:
    reverseLinkedList (Node 1 (Node 2 (Node 4 Empty)))
        -> (Node 4 (Node 2 (Node 1 Empty)))
-}
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList lst = convertListToLinked newLst
    where newLst = convertLinkedToList lst

convertLinkedToList :: LinkedList a -> [a]
convertLinkedToList lst = convertLinkedToListAux lst []

convertLinkedToListAux :: LinkedList a -> [a] -> [a]
convertLinkedToListAux Empty acc = acc
convertLinkedToListAux (Node x next) acc = convertLinkedToListAux next (x:acc)

convertListToLinked :: [a] -> LinkedList a
convertListToLinked [] = Empty
convertListToLinked (x:xs) = Node x (convertListToLinked xs)

{-
    19. Majority Element
    
    Given an [Int] of size n, return the majority element.
     
    The majority element is the element that appears more than n/2 times. 
    You may assume that the majority element always exists in the array.

    Test Cases:
    [3,2,3]
    [2,2,1,1,1,2,2]
-}
-- If majority always exist and it appears mor than n/2 time, 
-- then it locates at the mid of the sort list
majorityNum :: Ord a => [a] -> a
majorityNum xs = sortedXs !! mid
    where sortedXs = sort xs
          mid = length xs `div` 2

-- Work in more general environment. If no majority, return Nothing
majorityNumHash :: Ord a => [a] -> Maybe a
majorityNumHash xs = majorityNumHashAux xs (length xs `div` 2) M.empty

majorityNumHashAux :: Ord a => [a] -> Int -> M.Map a Int -> Maybe a
majorityNumHashAux [] _ _ = Nothing
majorityNumHashAux (x:xs) mlen dict
    | x `M.member` dict = if v+1 > mlen 
                          then Just x 
                          else majorityNumHashAux xs mlen newDict
    | otherwise = majorityNumHashAux xs mlen (M.insert x 1 dict)
    where Just v = M.lookup x dict
          newDict = M.insertWith (+) x 1 dict

-- Boyer–Moore majority vote algorithm
majorityNumBM :: Eq a => [a] -> Maybe a
majorityNumBM xs = majorityNumBMAux xs (head xs) 0

majorityNumBMAux :: Eq a => [a] -> a -> Int -> Maybe a
majorityNumBMAux [] x count = if count < 1 then Nothing else Just x
majorityNumBMAux (y:ys) y' count
    | y == y' = majorityNumBMAux ys y (count+1)
    | count == 0 = majorityNumBMAux ys y 1
    | otherwise = majorityNumBMAux ys y' (count-1)

{-
    20. Add Binary

    Given two binary strings a and b, return their sum as a binary string.
    
    Test Cases:
    addBinary (Binary "11")   (Binary "1")    -> Binary "100"
    addBinary (Binary "1010") (Binary "1011") -> Binary "10101"
-}
data Binary = Binary String deriving Show

{-
    Divide the number by 2.
    Get the integer quotient for the next iteration.
    Get the remainder for the binary digit.
    Repeat the steps until the quotient is equal to 0
-}
digitToBinary :: Int -> Binary
digitToBinary n
    | divided == 0 = Binary (show moded)
    | otherwise = Binary (nextBinary ++ show moded)
    where divided = n `div` 2
          moded = n `mod` 2
          Binary nextBinary = digitToBinary divided

-- decimal = d_0 * 2^0 + d_1 * 2^1 + d_2 * 2^2 + ...
binaryToDigit :: Binary -> Int
binaryToDigit (Binary bx) = binaryToDigitAux (Binary bx) (length bx - 1)

binaryToDigitAux :: Binary -> Int -> Int
binaryToDigitAux (Binary []) exp = 0
binaryToDigitAux (Binary (x:xs)) exp = 
    (digitToInt x) * 2 `powerOf` exp + binaryToDigitAux (Binary xs) (exp-1)

powerOf :: Int -> Int -> Int
powerOf base times
    | times == 0 = 1
    | otherwise = base * (powerOf base (times-1))

addBinary :: Binary -> Binary -> Binary
addBinary x y = digitToBinary (digitX + digitY)
    where digitX = binaryToDigit x
          digitY = binaryToDigit y

{-
    21. Diameter of Binary Tree

    Given the root of a binary tree, return the length of the diameter 
    of the tree. The diameter of a binary tree is the length of the 
    longest path between any two nodes in a tree. This path may or may 
    not pass through the root.

    The length of a path between two nodes is represented by the number 
    of edges between them.

    Test Case: 
    diameter tree5 -> 6
-}
tree5 = Node' 6 
    (Node' 2 
        (Node' 0 (Empty', 
                  Node' 7 (Node' 10 (Empty', Empty'), Empty')), 
         Node' 4 (Node' 3 (Empty', Empty'), 
                  Node' 5 (Node' 9 (Empty', Empty'), Empty'))), 
     Node' 8 (Empty', Empty'))
-- O(n)
diameter :: BiTree a -> Int
diameter tr = fst $ nodeDetail tr

nodeDetail :: BiTree a -> (Int, Int) -- (diameter,the longest path)
nodeDetail Empty' = (0,0)
nodeDetail (Node' _ (Empty', Empty')) = (0,0)
nodeDetail (Node' _ (l, Empty')) = incPair $ nodeDetail l
nodeDetail (Node' _ (Empty', r)) = incPair $ nodeDetail r
nodeDetail (Node' _ (l, r))
    | 2 + (lp1 + lp2) > max dia1 dia2 = (2 + (lp1 + lp2), max lp1 lp2 + 1)
    | otherwise = (max dia1 dia2, max lp1 lp2 + 1)
    where (dia1, lp1) = nodeDetail l
          (dia2, lp2) = nodeDetail r

incPair :: (Int, Int) -> (Int, Int)
incPair (x,y) = (x+1, y+1)

{-
    22. Middle of the Linked List

    Given the head of a singly linked list, return the middle node of 
    the linked list.
    
    If there are two middle nodes, return the second middle node.

    Test Cases:
    (Node 1 (Node 2 (Node 4 Empty))) -> Node 2 (Node 4 Empty)

    (Node 1 (Node 2 (Node 4 (Node 5 Empty)))) -> Node 4 (Node 5 Empty)
-}
-- get the length, then traverse to 1/2 length
midListWithLen :: LinkedList a -> LinkedList a
midListWithLen lst = midListWithLenAux lst 0 (lengthOf lst `div` 2) 

midListWithLenAux :: LinkedList a -> Int -> Int -> LinkedList a
midListWithLenAux Empty _ _ = error "Wrong initial mid length" 
midListWithLenAux (Node v next) i mid = 
    if i < mid then midListWithLenAux next (i+1) mid else Node v next

lengthOf :: LinkedList a -> Int
lengthOf Empty = 0
lengthOf (Node _ next) = 1 + lengthOf next

-- store LinkedList into Array, find th second half array, then recreate LinkedList
midListWithArr :: LinkedList a -> LinkedList a
midListWithArr lst = convertListToLinked r
    where arr = reverse $ convertLinkedToList lst
          mid = length arr `div` 2
          (_,r) = splitAt mid arr

-- Traverse the LinkedList with a track value, but only update the track value 1/2 times
midListWithTwoHead :: LinkedList a -> LinkedList a
midListWithTwoHead lst = midListWithTwoHeadAux lst lst False

midListWithTwoHeadAux :: LinkedList a -> LinkedList a -> Bool -> LinkedList a
midListWithTwoHeadAux Empty res _ = res
midListWithTwoHeadAux (Node _ _) Empty _ = error "Wrong initial value"
midListWithTwoHeadAux (Node _ next1) (Node v2 next2) isInc
    | isInc = midListWithTwoHeadAux next1 next2 (not isInc)
    | otherwise = midListWithTwoHeadAux next1 (Node v2 next2) (not isInc)

{-
    23. Maximum Depth of Binary Tree

    Given the root of a binary tree, return its maximum depth.

    A binary tree's maximum depth is the number of nodes along the longest 
    path from the root node down to the farthest leaf node. 
-}
maxDepthOf :: BiTree a -> Int
maxDepthOf tr = maximum $ allDepths tr

allDepths :: BiTree a -> [Nat]
allDepths tr = leavesDepthHelper tr 0

{-
    24. Contains Duplicate

    Given an array, return true if any value appears at least twice 
    in the array, and return false if every element is distinct.
-}

-- Hash version
isDuplicate :: Eq a => [a] -> Bool
isDuplicate lst = error "NI"

isDuplicateAux :: Eq a => [a] -> M.Map a Int -> Bool
isDuplicateAux lst dict = error "NI"

-- Sorting version

{-
    25. Maximum Sub-array
-}

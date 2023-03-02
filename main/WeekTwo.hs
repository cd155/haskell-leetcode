module WeekWeek where

import qualified Data.Map as M
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

findBadVer :: Maybe Int
findBadVer = if isBadVersion res then Just res else Nothing
    where res = findBadVerHelper (0, length testVersions -1)

findBadVerHelper :: (Int, Int) -> Int
findBadVerHelper (start,end)
    | start == end = start
    | isBadVersion (start+mid) = findBadVerHelper (start,start+mid)
    | otherwise = findBadVerHelper (start+mid+1, end)
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
    findLongestPalin "abccccdd" -> 7
    findLongestPalin "a" -> 1
-}
findLongestPalin :: String -> Int
findLongestPalin s1 
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
        -> (Node 4 (Node 2 (Node 1 Empty))
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
-}

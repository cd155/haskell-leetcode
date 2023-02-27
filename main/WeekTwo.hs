module WeekWeek where

import qualified Data.Map as M

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

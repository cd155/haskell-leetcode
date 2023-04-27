module WeekSix where

{-
  50. Word Break

  Given a string s and a dictionary of strings wordDict, return true if s 
  can be segmented into an sequence of one or more dictionary words.

  Note that the same word in the dictionary may be reused multiple times in 
  the segmentation.

  Test Cases:

  wordBreak "leetcode"      ["leet","code"]                   -> True
  wordBreak "applepenapple" ["apple","pen"]                   -> True
  wordBreak "catsandog"     ["cats","dog","sand","and","cat"] -> False
-}

wordBreak :: String -> [String] -> Bool
wordBreak str dict = wordBreakAux str [] dict

-- word is the build up word, once it is in dict, we set it to empty
-- in the base case, make sure check if there is any leftover
wordBreakAux :: String -> String -> [String] -> Bool
wordBreakAux [] word _ = if null word then True else False
wordBreakAux (x:xs) word dict
  | (word++[x]) `elem` dict = wordBreakAux xs [] dict
  | otherwise = wordBreakAux xs (word++[x]) dict

{-
  51. Partition Equal Subset Sum

  Given an [Int], return true if you can partition the array into two 
  subsets such that the sum of the elements in both subsets is equal or 
  otherwise false

  Test Cases:
  partEqualSetSum [1,11 5]    -> 
    [[1],[11,1],[5,1],[5,11,1],[11],[5,11],[5]]
  partEqualSetSum [1,2,3,4]   -> 
  
  partEqualSetSum' [1,5,11,5] -> True
  partEqualSetSum' [1,2,3,5]  -> False
-}

-- return all possible subsets
partEqualSetSum :: [Int] -> [[Int]] 
partEqualSetSum [] = []
partEqualSetSum (x:xs) = 
  partEqualSetSumAux [([x],0)] xs ++ partEqualSetSum xs

{-
  partEqualSetSumAux [([1],0)] [2,3,4]

  cur: current list
  i: the skip index, once one element be used, it will become unavailable
  ys: available elements 
-}
partEqualSetSumAux :: [([Int],Int)] -> [Int] -> [[Int]]
partEqualSetSumAux [] _ = []
partEqualSetSumAux ((cur,i):xs) ys = 
  cur: (partEqualSetSumAux (xs ++ allComb) ys)
  where allComb = zip (map (\y -> y:cur) (drop i ys)) [(i+1)..length ys]


partEqualSetSum' :: [Int] -> Bool
partEqualSetSum' xs 
  | odd sumXs = False
  | otherwise = partEqualSetSumLoop' xs (sumXs `div` 2)
  where sumXs = sum xs

partEqualSetSumLoop' :: [Int] -> Int -> Bool
partEqualSetSumLoop' [] target = False
partEqualSetSumLoop' (x:xs) target = 
  partEqualSetSumAux' [([x],0)] xs target || partEqualSetSumLoop' xs target

partEqualSetSumAux' :: [([Int],Int)] -> [Int] -> Int -> Bool
partEqualSetSumAux' [] _ _ = False
partEqualSetSumAux' ((cur,i):xs) ys target = 
  if sum cur == target then True else partEqualSetSumAux' (xs ++ allComb) ys target
  where allComb = zip (map (\y -> y:cur) (drop i ys)) [(i+1)..length ys]

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

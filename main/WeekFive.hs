module WeekFive where

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

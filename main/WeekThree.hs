module WeekThree where

{-
    26. Insert Interval

    You are given an ACS-sorted array of non-overlapping intervals intervals 
    Insert a new intervals that reminds the current structure

    Requirement: 
    1. still ASC-sorted array
    2. No over-lapping intervals

    insertInterval [(4,6),(8,10)] (12,14) -> [(4,6),(8,10),(12,14)]
    insertInterval [(4,6),(8,10)] (1,2) -> [(1,2),(4,6),(8,10)]
    insertInterval [(4,6),(8,10)] (8,9) -> [(4,6),(8,10)]
    insertInterval [(4,6),(8,10)] (3,10) -> [(3,10)]
    insertInterval [(4,6),(8,10)] (1,5) -> [(1,6),(8,10)]
    insertInterval [(4,6),(8,10)] (9,12) -> [(4,6),(8,12)]
-}
insertInterval :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
insertInterval [] interval = [interval]
insertInterval ((x1, x2):xs) (insert1, insert2)
    | insert1 >= x1 && insert2 <= x2 = (x1, x2):xs
    | insert2 < x1 = (insert1, insert2): (x1, x2): xs
    | insert1 > x2 = (x1, x2): insertInterval xs (insert1, insert2)
    | otherwise = if insert1 >= x1 
                  then (x1, right): rest
                  else (insert1, right): rest
    where (right, rest) = lookForRight ((x1, x2): xs) insert2

lookForRight :: [(Int, Int)] -> Int -> (Int, [(Int, Int)])
lookForRight [] right = (right, [])
lookForRight ((x1, x2):xs) right 
    | right < x1 = (right, xs)
    | right < x2 = (x2, xs)
    | otherwise = lookForRight xs right

{-
    27. 01 Matrix

    Given an m x n binary matrix mat, 
    return the distance of the nearest 0-value cell for each cell.

    The distance of the nearest 0 for a cell in the binary matrix 
    is the minimum number of steps needed to reach the nearest 
    0-valued cell from that cell, where a step is a movement from 
    one adjacent cell to another (not diagonally).
    
    1. 1-1 has distance 1
    2. 1-0 or 0-1 has distance 1
-}

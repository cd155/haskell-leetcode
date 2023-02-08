module WeekOne where
import Prelude hiding (lookup)
import Data.Map

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

twoSumHashHelper :: [Integer] -> Integer -> Map Integer [Nat] -> (Nat, Nat)
twoSumHashHelper [] tar dict = error "No two number add up to the target"
twoSumHashHelper (x:xs) tar dict 
    | x == y && length indX > 1 = (indX!!1, indX!!0) -- format (smaller #, larger #)
    | x == y && length indX <= 1 = twoSumHashHelper xs tar dict
    | y `member` dict = (head indX, head indY)
    | otherwise = twoSumHashHelper xs tar dict
    where y = tar - x
          Just indX = lookup x dict
          Just indY = lookup y dict

-- convert list to a hash table with indexes as values
convToDict :: [Integer] -> Map Integer [Nat]
convToDict xs = convToDictHelper xs empty 0

convToDictHelper :: [Integer] -> Map Integer [Nat] -> Nat -> Map Integer [Nat]
convToDictHelper [] dict _ = dict
convToDictHelper (x:xs) dict i
    | x `member` dict = convToDictHelper xs (insertWith (++) x [i] dict) (i+1)
    | otherwise = convToDictHelper xs (insert x [i] dict) (i+1)  

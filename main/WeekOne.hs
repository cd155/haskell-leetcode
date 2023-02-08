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

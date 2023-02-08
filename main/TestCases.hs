import WeekOne

-- Two sum
sampleTS = 
    [
        [2,7,11,15],
        [3,2,4],
        [3,3],
        [5,2,3,5,1,7,1]
    ]

targetTS = [9,6,6,2]

answerTS = 
    [
        (0,1),
        (1,2),
        (0,1),
        (4,6)
    ]

runTwoSumTest :: [[Integer]] -> [Integer]-> [(Nat,Nat)] -> Bool
runTwoSumTest [] _ _ = True
runTwoSumTest _ [] _ = True
runTwoSumTest _ _ [] = True
runTwoSumTest (x:xs) (y:ys) (z:zs)
    | (twoSumBF x y) == z && (twoSumHash x y) == z = 
        True && runTwoSumTest xs ys zs
    | otherwise = error (
        "Input: " ++ show (x,y) ++ 
        "is not match with Output: "++ show z)

sampleIVP = ["()", "()[]{}", "([{()}{}])", "(]", "(([][])[})"]
answerIVP = [True, True, True, False, False]

runIsValidParen :: [String] -> [Bool] -> Bool
runIsValidParen [] _ = True
runIsValidParen _ [] = True
runIsValidParen (x:xs) (y:ys)
    | isValidParen x == y = True && runIsValidParen xs ys
    | otherwise = error (
        "Input: " ++ show (x) ++ 
        "is not match with Output: "++ show y)

testAll = do
    let ts = runTwoSumTest sampleTS targetTS answerTS
    let ivp = runIsValidParen sampleIVP answerIVP

    if ts && ivp then
        putStrLn "All cases passed."
    else
        putStrLn $ show ts
    
    putStrLn "----End----"

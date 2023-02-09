import WeekOne

-- Two sum
sampleTwoSum = 
    [
        ([2,7,11,15], 9),
        ([3,2,4], 6),
        ([3,3], 6),
        ([5,2,3,5,1,7,1],2)
    ]

answerTwoSum = 
    [
        (0,1),
        (1,2),
        (0,1),
        (4,6)
    ]

runTwoSumTest :: [([Integer],Integer)]-> [(Nat,Nat)] -> Bool
runTwoSumTest [] _ = True
runTwoSumTest _ [] = True
runTwoSumTest (x:xs) (y:ys)
    | (twoSumBF (fst x) (snd x)) == y && (twoSumHash (fst x) (snd x)) == y = 
        True && runTwoSumTest xs ys
    | otherwise = error (
        "Input: " ++ show (fst x,snd x) ++ 
        "is not match with Output: "++ show y)

sampleIsValidParen = ["()", "()[]{}", "([{()}{}])", "(]", "(([][])[})"]
answerIsValidParen = [True, True, True, False, False]

runIsValidParen :: [String] -> [Bool] -> Bool
runIsValidParen [] _ = True
runIsValidParen _ [] = True
runIsValidParen (x:xs) (y:ys)
    | isValidParen x == y = True && runIsValidParen xs ys
    | otherwise = error (
        "Input: " ++ show (x) ++ 
        "is not match with Output: "++ show y)

testAll = do
    let ts = runTwoSumTest sampleTwoSum answerTwoSum
    let ivp = runIsValidParen sampleIsValidParen answerIsValidParen

    if ts && ivp then
        putStrLn "All cases passed."
    else
        putStrLn $ show ts
    
    putStrLn "----End----"

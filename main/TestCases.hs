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

testAll = do
    let ts = runTwoSumTest sampleTS targetTS answerTS
    if ts then
        putStrLn "All Two Sum cases passed."
    else
        putStrLn $ show ts
    
    putStrLn "----End----"

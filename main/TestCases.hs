import WeekOne

-- Two sum
sampleTS = 
    [
        [2,7,11,15],
        [3,2,4],
        [3,3]
    ]

targetTS = [9,6,7]

answerTS = 
    [
        (0,1),
        (1,2),
        (0,1)
    ]

runTSTest :: [[Integer]] -> [Integer]-> [(Nat,Nat)] -> Bool
runTSTest [] _ _ = True
runTSTest _ [] _ = True
runTSTest _ _ [] = True
runTSTest (x:xs) (y:ys) (z:zs)
    | (twoSumBF x y) == z = True && runTSTest xs ys zs
    | otherwise = error (
        "Input: " ++ show (x,y) ++ 
        "is not match with Output: "++ show y)

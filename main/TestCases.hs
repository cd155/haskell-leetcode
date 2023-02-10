import WeekOne

-- Two sum
sampleTwoSum = [
    ([2,7,11,15], 9),
    ([3,2,4], 6),
    ([3,3], 6),
    ([5,2,3,5,1,7,1],2)]

answerTwoSum = [
    (0,1),
    (1,2),
    (0,1),
    (4,6)]

runTwoSumTest :: [([Integer],Integer)]-> [(Nat,Nat)] -> Bool
runTwoSumTest [] _ = True
runTwoSumTest _ [] = True
runTwoSumTest (x:xs) (y:ys)
    | (twoSumBF (fst x) (snd x)) == y && (twoSumHash (fst x) (snd x)) == y = 
        True && runTwoSumTest xs ys
    | otherwise = error (
        "Input: " ++ show (fst x, snd x) ++ 
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

sampleMergeLists = [
    (Node 1 (Node 2 (Node 4 Empty)), Node 1 (Node 3 (Node 4 Empty))),
    (Empty, Empty),
    (Empty, Node 0 Empty)]

answerMergeLists = [
    Node 1 (Node 1 (Node 2 (Node 3 (Node 4 (Node 4 Empty))))),
    Empty,
    Node 0 Empty]

runMergeLists :: Ord a => Show a => 
    [(LinkedList a, LinkedList a)] -> [LinkedList a] -> Bool
runMergeLists [] _ = True
runMergeLists _ [] = True
runMergeLists (x:xs) (y:ys)
    | (mergeLists (fst x) (snd x)) == y && (mergeLists' (fst x) (snd x)) == y = True && runMergeLists xs ys
    | otherwise = error (
        "Input: " ++ show (fst x, snd x) ++ 
        "is not match with Output: "++ show y)

sampleSellStocks :: [[Int]]
sampleSellStocks =[
    [7,1,5,3,6,4],
    [7,6,4,3,1],
    [8,7,1,5,3,6,7,0,5,10]]

answerSellStock = [5,0,10]

runSellStocks :: [[Int]] -> [Int] -> Bool
runSellStocks [] _ = True
runSellStocks _ [] = True
runSellStocks (x:xs) (y:ys) 
    | (maxProfit x == y) && (maxProfit' x == y) = True && runSellStocks xs ys
    | otherwise = error (
        "Input: " ++ show (x) ++ 
        "is not match with Output: "++ show y)

testAll = do
    let ts = runTwoSumTest sampleTwoSum answerTwoSum
    let ivp = runIsValidParen sampleIsValidParen answerIsValidParen
    let ml = runMergeLists sampleMergeLists answerMergeLists
    let ss = runSellStocks sampleSellStocks answerSellStock

    if ts && ivp && ml && ss then
        putStrLn "All cases passed."
    else
        putStrLn $ show (ts, ivp, ml)
    
    putStrLn "----End----"

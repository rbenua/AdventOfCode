import System.IO
import System.Environment
import Debug.Trace
import Data.List (find)

split :: Eq a => a -> [a] -> [[a]]
split sep [] = []
split sep l =
    let (first, rest) = span (/= sep) l
        r = case rest of
            [] -> []
            _:xs -> xs
    in first:split sep r 

main :: IO ()
main = do
    args <- getArgs 
    call_line:_:board_lines <- fmap lines $ readFile $ head args
    let calls = map read $ split ',' call_line :: [Int]
    let boards = map readBoard $ split "" board_lines
    putStr "Part 1: "
    print $ score <$> firstWin boards [] calls
    putStr "Part 2: "
    print $ score <$> lastWin boards [] calls
    return ()

readBoard :: [String] -> [[Int]]
readBoard = map (map read . words)

win :: [Int] -> [[Int]] -> Bool
win called board = 
    let row_win = any (all (`elem` called)) board
    in row_win || colWin called board

colWin :: [Int] -> [[Int]] -> Bool
colWin called board =
    let (first_col, rest) = unzip $ map (\(x:xs) -> (x, xs)) board
        f = all (`elem` called) first_col
    in if null (head rest)
    then f
    else f || colWin called rest

firstWin :: [[[Int]]] -> [Int] -> [Int] -> Maybe (Int, [Int], [[Int]])
firstWin boards prev_calls [] = Nothing 
firstWin boards prev_calls (call:rest) =
    let called = call:prev_calls
        res = find (win called) boards
    in case res of
        Nothing -> firstWin boards called rest
        Just b -> Just (call, called, b)

score :: (Int, [Int], [[Int]]) -> Int
score (call, called, b) =
    call * sum [e | e <- concat b, e `notElem` called]

lastWin :: [[[Int]]] -> [Int] -> [Int] -> Maybe (Int, [Int], [[Int]])
lastWin boards prev_calls [] = Nothing 
lastWin boards prev_calls (call:rest) =
    let called = call:prev_calls
        remaining = filter (not . win called) boards
    in case remaining of
        [] -> Nothing 
        [b] -> findWin b called rest
        _ -> lastWin remaining called rest

findWin :: [[Int]] -> [Int] -> [Int] -> Maybe (Int, [Int], [[Int]])
findWin b prev_calls [] = Nothing
findWin b prev_calls (call:rest) =
    let called = call:prev_calls
    in if win called b
    then Just (call, called, b)
    else findWin b called rest
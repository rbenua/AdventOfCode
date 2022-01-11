import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit )

search :: (Char -> Bool) -> String -> [String]
search pred [] =  []
search pred s =
    let dp = dropWhile (not . pred) s
        (ns, rest) = span pred dp
    in if null ns
    then search pred rest
    else ns : search pred rest

split :: Eq a => a -> [a] -> [[a]]
split sep [] = []
split sep l =
    let (first, rest) = span (/= sep) l
        r = case rest of
            [] -> []
            _:xs -> xs
    in first:split sep r 

repl :: Integral n => n -> (a -> a) -> a -> a
repl 0 _ v = v
repl n f v = repl (n - 1) f (f v)


main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ case args of
        [] -> "input.txt"
        name:_ -> name
    let rows = lines input
    putStr "Part 1: "
    print $ map (foldl step1 '5') rows
    putStr "Part 2: "
    print $ map (foldl step2 '5') rows
    return ()

dirIndex c = case c of
    'U' -> 0
    'R' -> 1
    'D' -> 2
    'L' -> 3
    _ -> error "bad direction"

step1 curr c = 
    (case curr of
        '1' -> ['1', '2', '4', '1']
        '2' -> ['2', '3', '5', '1']
        '3' -> ['3', '3', '6', '2']
        '4' -> ['1', '5', '7', '4']
        '5' -> ['2', '6', '8', '4']
        '6' -> ['3', '6', '9', '5']
        '7' -> ['4', '8', '7', '7']
        '8' -> ['5', '9', '8', '7']
        '9' -> ['6', '9', '9', '8']
        _ -> error "bad starting point"
    ) !! dirIndex c

step2 curr c =
    (case curr of
        '1' -> ['1', '1', '3', '1']
        '2' -> ['2', '3', '6', '2']
        '3' -> ['1', '4', '7', '2']
        '4' -> ['4', '4', '8', '3']
        '5' -> ['5', '6', '5', '5']
        '6' -> ['2', '7', 'A', '5']
        '7' -> ['3', '8', 'B', '6']
        '8' -> ['4', '9', 'C', '7']
        '9' -> ['9', '9', '9', '8']
        'A' -> ['6', 'B', 'A', 'A']
        'B' -> ['7', 'C', 'D', 'A']
        'C' -> ['8', 'C', 'C', 'B']
        'D' -> ['B', 'D', 'D', 'D']
        _ -> error "bad starting point"
    ) !! dirIndex c
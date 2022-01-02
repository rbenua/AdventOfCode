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

repl :: (Eq t1, Num t1) => t1 -> (t2 -> t2) -> t2 -> t2
repl 0 _ v = v
repl n f v = repl (n - 1) f (f v)


main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ case args of
        [] -> "input.txt"
        name:_ -> name
    let posns = map read $ search isDigit input
    putStr "Part 1:"
    print $ minDist dist1 posns
    putStr "Part 2:"
    print $ minDist dist2 posns
    return ()

dist1 :: Int -> Int -> Int 
dist1 a b = abs (a - b)

dist2 :: Int -> Int -> Int 
dist2 a b = 
    let n = dist1 a b
    in (n * (n + 1)) `div` 2

minDist :: (Int -> Int -> Int) -> [Int] -> Int
minDist df ps = 
    let range = [(minimum ps)..(maximum ps)]
    in minimum $ map (\x -> sum $ map (df x) ps) range
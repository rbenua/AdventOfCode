import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit )
import Data.List (transpose)

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
    let tris = map (map read . words) $ lines input
    putStr "Part 1: "
    print $ countValid $ concat tris
    putStr "Part 2: "
    print $ countValid $ concat $ transpose tris
    return ()

validTri a b c =
    a + b > c &&
    a + c > b &&
    b + c > a

countValid [] = 0
countValid tris =
    let ([a, b, c], rest) = splitAt 3 tris
    in (countValid rest) + if validTri a b c then 1 else 0
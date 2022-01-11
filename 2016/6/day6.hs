import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit, isAlpha )
import Data.List (transpose)
import qualified Data.Map as M
import Data.Tuple (swap)

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
    let columns = transpose $ lines input
    putStrLn $ "Part 1: " ++ map mostCommon columns
    putStrLn $ "Part 2: " ++ map leastCommon columns
    return ()

mostCommon :: Ord b => [b] -> b
mostCommon cs =
    let freqs = M.fromListWith (+) $ zip cs (repeat 1)
    in snd $ maximum $ map swap $ M.toList freqs

leastCommon :: Ord b => [b] -> b
leastCommon cs =
    let freqs = M.fromListWith (+) $ zip cs (repeat 1)
    in snd $ minimum $ map swap $ M.toList freqs
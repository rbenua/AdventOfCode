import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit )
import qualified Data.Map as M

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

repl :: Integral n => Show a => n -> (a -> a) -> a -> a
repl 0 _ v = v
repl n f v = repl (n - 1) f (f v)

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ case args of
        [] -> "input.txt"
        name:_ -> name
    let starters = map read $ search isDigit input :: [Int]
    let freqs = M.fromListWith (+) $ zip starters $ repeat 1
    print freqs
    putStr "Part 1: "
    print $ M.foldr (+) 0 $ repl 80 step freqs
    putStr "Part 2: "
    print $ M.foldr (+) 0 $ repl 256 step freqs
    return ()

krotate :: (Eq p, Num p) => p -> p
krotate 0 = 8
krotate k = k - 1

rotate :: M.Map Int Int -> M.Map Int Int
rotate = M.mapKeys krotate

step :: M.Map Int Int -> M.Map Int Int
step m = M.insertWith (+) 6 (M.findWithDefault 0 0 m) $ rotate m

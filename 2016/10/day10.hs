import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (isPrefixOf)

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

filterSplit :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
filterSplit pred = foldr (\e (ts, fs) -> if pred e then (e:ts, fs) else (ts, e:fs)) ([], [])

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ case args of
        [] -> "input.txt"
        name:_ -> name
    return ()

data Node = Input Int | Bot Int | Output Int deriving (Eq, Ord, Show)

forwardEdges :: String -> [(Node, S.Set Node)]
forwardEdges line | "input" `isPrefixOf` line =
    let [in_id, bot_id] = map read $ search isDigit line
    in [(Input in_id, S.singleton $ Bot bot_id)]
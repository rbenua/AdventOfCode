import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit, isAlpha )
import qualified Data.Set as S

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
    let fulls = map (map S.fromList . search isAlpha) $ lines input
    let lines = map (\x -> (x, drop (length x - 4) x)) fulls
    putStr "Part 1:"
    print $ sum $ map (sum . map (maybe 0 (const 1) . unique) . snd) lines
    return ()

unique :: S.Set Char -> Maybe Int
unique s = case length s of
    2 -> Just 1
    3 -> Just 7
    4 -> Just 4
    7 -> Just 8
    _ -> Nothing

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

filterSplit :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
filterSplit pred = foldr (\e (ts, fs) -> if pred e then (e:ts, fs) else (ts, e:fs)) ([], [])

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ case args of
        [] -> "input.txt"
        name:_ -> name
    putStr "Part 1: "
    print $ sum $ map (length . expand) $ lines input
    putStr "Part 2: "
    print $ sum $ map (expLen 0 (1, [])) $ lines input
    return ()

expand :: String -> String 
expand [] = []
expand ('(':rest) = 
    let (inner, _:outer) = break (== ')') rest
        [len, times] = map read $ search isDigit inner
        (rep, final) = splitAt len outer
    in concat (replicate times rep) ++ expand final
expand (c:rest) = c : expand rest

type ExpState = (Int, [(Int, Int)])

incMul :: ExpState -> Int -> ExpState
incMul (cur_mul, muls) amt =
    let subbed = map (\(a, m) -> (a - amt, m)) muls
        (to_remove, remaining) = filterSplit ((<= 0) . fst) subbed
        prod_remove = product $ map snd to_remove
    in (cur_mul `div` prod_remove, remaining)

expLen :: Int -> ExpState -> String -> Int
expLen prev_len _ [] = prev_len
expLen prev_len p@(cur_mul, muls) (c:cs) =
    if c /= '('
    then expLen (prev_len + cur_mul) (incMul p 1) cs
    else let 
            (marker, _:rest) = break (== ')') cs
            [amt, mul] = map read $ search isDigit marker :: [Int]
            mlen = length marker + 2
            (mend_mul, mend_muls) = incMul p mlen
            np = (mend_mul * mul, (amt, mul):mend_muls)
        in expLen prev_len np rest
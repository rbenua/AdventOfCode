import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit, isAsciiLower, isLower )
import qualified Data.Map.Strict as M
import Data.Tuple (swap)
import Data.List (sort, sortBy, elemIndex)

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
    let valids = filter cksum $ lines input
    putStr "Part 1: "
    print $ sum $ map idNum valids
    putStr "Part 2: "
    print $ idNum $ head $ filter ((== "northpole") . take 9) $ map rotate valids
    return ()

cksum :: [Char] -> Bool
cksum line =
    let c = last $ search isLower line
        f = sort $ map swap $ M.toList $ freqs line
        c' = map snd (take 5 f)
    in c == c'

freqs :: Num a => [Char] -> M.Map Char a
freqs line =
    let l = filter isLower $ head $ split '[' line
    in M.fromListWith (+) $ zip l $ repeat (-1)

idNum :: String -> Int
idNum = read . head . search isDigit

alpha = ['a'..'z']

rotChar :: Int -> Char -> Char
rotChar amt c =
    if isLower c then
        let Just n = elemIndex c alpha
        in alpha !! ((n + amt) `mod` 26)
    else c

rotate :: String -> String
rotate line = map (rotChar (idNum line)) line
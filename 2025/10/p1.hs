{- HLINT ignore "Use map with tuple-section" -}
import System.IO
import Data.List
import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Array
import Data.List.Split
import Data.Char
import Data.MemoTrie
import Debug.Trace
import Data.Maybe
import Data.Ord
import Data.Bits

main = do
    args <- getArgs
    contents <- readFile (head args)
    let problems = map parseLine $ lines contents
        results = map search problems
    print results
    print $ sum <$> sequence results

parseLine :: String -> (Int, [Int])
parseLine l =
    let start:blocks = splitOn " " l
        pstart = sum [(if c == '#' then 1 else 0) `shiftL` i | (i, c) <- zip [0..] $ filter (`elem` ".#") start]
        parse_block = sum . map ((1 `shiftL`) . read . filter isDigit) . splitOn ","
        pblocks = map parse_block $ take (length blocks - 1) blocks
    in (pstart, pblocks)

-- all subsequences of length n
subsLength :: (Eq t, Num t) => [a] -> t -> [[a]]
subsLength [] n = []
subsLength _ 0 = []
subsLength ls 1 = map singleton ls
subsLength (l:ls) n = map (l:) (subsLength ls (n - 1)) ++ subsLength ls n

-- all subsequences in ascending length order
subsByLength :: [a] -> [[a]]
subsByLength ls = concatMap (subsLength ls) [1..length ls]

search (goal, switches) = fmap length $ find (\s -> foldl xor 0 s == goal) $ subsByLength switches
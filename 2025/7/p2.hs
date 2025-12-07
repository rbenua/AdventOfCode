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

main = do
    args <- getArgs
    contents <- readFile (head args) 
    let start_col = head $ elemIndices 'S' $ head $ lines contents
        columns = parse $ lines contents
    print $ memoFix (solve columns) (start_col, 0)


parse :: [String] -> Array Int [Int]
parse ls =
    let maxx = length $ head ls
        maxy = length ls
        tls = transpose ls
        parsecol l = [i | (i, x) <- zip [0..] l, x == '^']
    in array (0, maxx - 1) $ zip [0..] $ map parsecol tls

solve :: Array Int [Int] -> ((Int, Int) -> Int) -> (Int, Int) -> Int
solve columns solve' (start_col, start_row) =
    case find (> start_row) (columns ! start_col) of
        Nothing -> 1
        Just row -> solve' (start_col - 1, row) + solve' (start_col + 1, row)
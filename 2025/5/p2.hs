{- HLINT ignore "Use map with tuple-section" -}
import System.IO
import Data.List
import System.Environment (getArgs)
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Array
import Data.List.Split

main = do
    args <- getArgs
    contents <- readFile (head args) 
    let [ranges, vals] = splitOn "\n\n" contents
        iranges :: [[Integer]] = map (map read . splitOn "-") $ lines ranges
        ivals :: [Integer] = map read $ lines vals
        sranges = sortOn head iranges
    print $ total sranges

overlaps upper [] = (upper, [])
overlaps upper ([l, u]:rest) | l <= upper = overlaps (max u upper) rest
overlaps upper all = (upper, all)

total [] = 0
total ([lower, upper]:rest) =
    let 
        (curr_end, remaining) = overlaps upper rest
        curr = curr_end - lower + 1
    in curr + total remaining

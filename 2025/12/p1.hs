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
import Control.Monad.Fix (fix)

main = do
    args <- getArgs
    contents <- readFile (head args)
    let (shapes, [regions]) = splitAt 6 $ splitOn "\n\n" contents
        regsizes = map (length . filter (== '#')) shapes
        results = map (check regsizes) $ lines regions
    print $ sum <$> sequence results

--apparently my input is weak enough that this is good enough? 
--kind of defeats the point of the problem IMO...
check regsizes regline =
    let (dims:regs) = splitOn " " regline
        regcounts = map read regs
        [x, y] = map (read . filter isDigit) $ splitOn "x" dims
        area = x * y
        upper = 9 * sum regcounts
        lower = sum $ zipWith (*) regsizes regcounts
    in case (area < lower, area >= upper) of
        (True, _) -> Just 0
        (_, True) -> Just 1
        _ -> Nothing
        
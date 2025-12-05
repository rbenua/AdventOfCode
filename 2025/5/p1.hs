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
        result = length [i | i <- ivals, any (\[lower, upper] -> lower <= i && upper >= i) iranges]
    print result
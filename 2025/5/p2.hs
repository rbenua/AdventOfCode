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
    let [ranges, _] = splitOn "\n\n" contents
        iranges :: [[Integer]] = map (map read . splitOn "-") $ lines ranges
        [l,u]:sranges = sortOn head iranges
    print $ total l u sranges

total lower upper [] = upper - lower + 1
total lower upper ([l, u]:rest) | l <= upper = total lower (max u upper) rest
total lower upper ([l, u]:rest) = upper - lower + 1 + total l u rest
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

main = do
    args <- getArgs
    contents <- readFile (head args)
    let points :: [(Int, Int)] = map (\l -> read $ "(" ++ l ++ ")") $ lines contents
    print $ biggest points

area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

biggest [] = 0
biggest [a] = 0
biggest (a:rest) = max (maximum $ map (area a) rest) $ biggest rest
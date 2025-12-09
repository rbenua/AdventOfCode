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
    print $ biggest points ((last points):points)

area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

allPairs f [] = True
allPairs f [_] = True
allPairs f (x:y:rest) = f x y && allPairs f (y:rest)

ins (min, max) val = min < val && max > val

testLine (bx1, by1) (bx2, by2) (lx1, ly1) (lx2, ly2) =
    let (bxmin, bxmax) = (min bx1 bx2, max bx1 bx2)
        (bymin, bymax) = (min by1 by2, max by1 by2)
        inside1 = ins (bxmin, bxmax) lx1 && ins (bymin, bymax) ly1
        inside2 = ins (bxmin, bxmax) lx2 && ins (bymin, bymax) ly2
        crossx = (ly1 == ly2) && ins (bymin, bymax) ly1 && (min lx1 lx2) <= bxmin && (max lx1 lx2) >= bxmax
        crossy = (lx1 == lx2) && ins (bxmin, bxmax) lx1 && (min ly1 ly2) <= bymin && (max ly1 ly2) >= bymax
    in not inside1 && not inside2 && not crossx && not crossy

safe whole b1 b2 = allPairs (testLine b1 b2) whole

biggest [] _ = (0, (0, 0), (0, 0))
biggest (a:rest) whole = max (maximum $ (0, (0, 0), (0, 0)):(map (\b -> (area a b, a, b)) $ filter (safe whole a) rest)) $ biggest rest whole 
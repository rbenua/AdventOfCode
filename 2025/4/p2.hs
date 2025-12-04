{- HLINT ignore "Use map with tuple-section" -}
import System.IO
import Data.List
import System.Environment (getArgs)
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Array

main = do
    args <- getArgs
    contents <- readFile (head args) 
    let grid = lines contents
        maxx = length $ head grid
        maxy = length grid
        arrgrid = listArray ((0, 0), (maxy - 1, maxx - 1)) $ concat grid
        endgrid = iter (remove maxx maxy) arrgrid
    print $ countAts arrgrid
    print $ countAts endgrid 
    print $ countAts arrgrid - countAts endgrid

nbrs :: (Num a, Num b, Ord a, Ord b) => a -> b -> (a, b) -> [(a, b)]
nbrs maxx maxy (x, y) = [(x', y') | x' <- [x - 1, x, x + 1], y' <- [y-1, y, y + 1], 
                                    (x', y') /= (x, y), 
                                    0 <= x', x' < maxx, 0 <= y', y' < maxy] 

remove maxx maxy arrgrid =
    let to_remove = [(y, x) | x <- [0..maxx - 1], y <- [0..maxy - 1],
                            arrgrid ! (y, x) == '@',
                            length (filter (\(x', y') -> (arrgrid ! (y', x')) == '@') $ nbrs maxx maxy (x, y)) < 4]
    in
        arrgrid // zip to_remove (repeat '.')

countAts = foldl' (\c e -> c + if e == '@' then 1 else 0) 0

iter f a =
    let next = f a
    in if next == a
       then a
       else iter f next
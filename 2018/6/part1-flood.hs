import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char

-- pull out every part of a String that can be read in
-- for some Read a and ignore the rest
readOut :: Read a => String -> [a]
readOut "" = []
readOut s = case reads s of
  [] -> readOut $ tail s
  [(x, s')] -> x : readOut s'
  _ -> error "ambiguous parse"
ireadOut :: String -> [Int]
ireadOut = readOut

dist (a, b) (c, d) = (abs $ a - c) + (abs $ b - d)

closest pts p = snd $ minimum [(dist p e, e) | e <- S.elems pts]

boundary :: (Int, Int, Int, Int) -> [(Int, Int)]
boundary (minx, maxx, miny, maxy) = [(x, miny - 1) | x <- [minx..maxx]] ++ [(x, maxy + 1) | x <- [minx..maxx]]
                                 ++ [(minx - 1, y) | y <- [miny..maxy]] ++ [(maxx + 1, y) | y <- [miny..maxy]]

limits pts = let minx = S.findMin $ S.map fst pts
                 maxx = S.findMax $ S.map fst pts
                 miny = S.findMin $ S.map snd pts
                 maxy = S.findMax $ S.map snd pts
             in (minx, maxx, miny, maxy)

removeInf :: S.Set (Int, Int) -> S.Set (Int, Int)
removeInf pts = pts S.\\ (S.map (closest pts) $ S.fromList $ boundary $ limits pts)

nbrs (x, y) = S.fromList [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

toExplore p exclude start = S.filter p $ S.difference (nbrs start) exclude

flood p visited tovisit | S.null tovisit = visited 
flood p visited tovisit = let (next, rest) = S.deleteFindMin tovisit
                          in flood p (S.insert next visited) (S.union (toExplore p (S.union visited tovisit) next) rest)

isClosest p1 pts p2 = (closest pts p2) == p1

findSize pts p = S.size $ flood (isClosest p pts) S.empty (S.singleton p) 

parse s = let is = ireadOut s
          in (is !! 0, is !! 1)

go filename = do input <- readFile filename
                 let pts = S.fromList $ map parse $ lines input
                 print $ S.findMax $ S.map (findSize pts) (removeInf pts)

main = do args <- getArgs
          go $ head args

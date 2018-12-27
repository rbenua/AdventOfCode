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

parse s = let is = ireadOut s
          in (is !! 0, is !! 1)

addClosest pts m p = M.alter (\n -> Just $ (fromMaybe 0 n) + 1) (closest pts p) m

buildMap pts (minx, maxx, miny, maxy) = foldl (addClosest pts) M.empty [(x,y) | x <- [minx..maxx], y <- [miny..maxy]]

go filename = do input <- readFile filename
                 let pts = S.fromList $ map parse $ lines input
                 print $ maximum $ map snd $ M.toList $ M.restrictKeys (buildMap pts $ limits pts) (removeInf pts)

main = do args <- getArgs
          go $ head args

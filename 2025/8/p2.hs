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
    let points :: [Point] = map (\l -> read $ "(" ++ l ++ ")") $ lines contents
        dists = sort $ allDists points
    print $ part2 Map.empty dists $ length points

type Point = (Float, Float, Float)
type Graph = Map.Map Point (Set.Set Point)

dist :: Point -> Point -> Float
dist (x1, y1, z1) (x2, y2, z2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2

allDists :: [Point] -> [(Float, Point, Point)]
allDists [] = []
allDists (x:rest) = [(dist x y, x, y) | y <- rest] ++ allDists rest

addEdge ::  Graph -> (t, Point, Point) -> Graph
addEdge g (_, a, b) =
    let as = fromMaybe (Set.singleton a) $ Map.lookup a g
        bs = fromMaybe (Set.singleton b) $ Map.lookup b g
        u = Set.union as bs
    in foldr (\k g' -> Map.insert k u g') g u

part2 :: Graph -> [(t, Point, Point)] -> Int -> Integer
part2 g [] goal = -1
part2 g (p@(_, a@(ax, _, _), b@(bx, _, _)):rest) goal =
    let g' = addEdge g p
    in  if fmap Set.size (Map.lookup a g') == Just goal
        then round ax * round bx
        else part2 g' rest goal
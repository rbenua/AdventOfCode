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
        dists = take (read $ args !! 1) $  sort $ allDists points
        graph = foldl addEdge Map.empty dists
        comps = Set.fromList $ Map.elems graph
        sizes = sortOn Down $ map Set.size $ Set.toList comps
    print $ product $ take 3 sizes

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

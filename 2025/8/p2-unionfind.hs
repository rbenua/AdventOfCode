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
    print $ evalState (part2 dists $ length points) Map.empty

type Point = (Float, Float, Float)
type Graph t = Map.Map t (Node t)

data Node t = Root Int | Member t deriving (Ord, Eq, Show)

dist :: Point -> Point -> Float
dist (x1, y1, z1) (x2, y2, z2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2

allDists :: [Point] -> [(Float, Point, Point)]
allDists [] = []
allDists (x:rest) = [(dist x y, x, y) | y <- rest] ++ allDists rest

gfind :: Ord t => t -> State (Graph t) t
gfind i = do
    n <- gets $ Map.findWithDefault (Root 1) i
    case n of
        Root size -> return i
        Member next -> do
            r <- gfind next
            modify $ Map.insert i (Member r)
            return r

gunion :: (Ord t, Show t) => t -> t -> State (Graph t) Int
gunion a b = do
    na <- gfind a
    nb <- gfind b
    g <- get
    let Root sa = Map.findWithDefault (Root 1) na g
        Root sb = Map.findWithDefault (Root 1) nb g
        news = sa + sb
        (smaller, bigger) = if sa < sb then (na, nb) else (nb, na)
    if na == nb
    then return sa
    else do
        modify $ Map.insert smaller (Member bigger)
        modify $ Map.insert bigger (Root news)
        return news

part2 :: [(t, Point, Point)] -> Int -> State (Graph Point) Integer
part2 [] goal = return (-1)
part2 ((_, a@(ax, _, _), b@(bx, _, _)):rest) goal = do
    newsize <- gunion a b
    if newsize == goal
    then return $ round ax * round bx
    else part2 rest goal
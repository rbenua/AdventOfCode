import System.IO
import System.Environment
import Debug.Trace
import Data.Char (isDigit)
import qualified Data.Set as S

allNums :: String -> [Int]
allNums [] =  []
allNums s =
    let dp = dropWhile (not . isDigit) s
        (ns, rest) = span isDigit dp
    in read ns : allNums rest

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ case args of
        [] -> "input.txt"
        name:_ -> name
    let ls = map ((\[x1,y1,x2,y2] -> Line {p1=(x1,y1), p2=(x2,y2)}) . allNums) $ lines input
    putStr "Part 1: "
    print $ S.size $ allIntersections S.empty $ map allPts $ filter ortho ls
    putStr "Part 2: "
    print $ S.size $ allIntersections S.empty $ map allPts ls
    return ()

data Line = Line {
    p1 :: (Int, Int),
    p2 :: (Int, Int)
} deriving Show

ortho :: Line -> Bool
ortho line = fst (p1 line) == fst (p2 line) || snd (p1 line) == snd (p2 line)

pt :: Line -> Bool
pt line = fst (p1 line) == fst (p2 line) && snd (p1 line) == snd (p2 line)

allPts :: Line -> S.Set (Int, Int)
allPts l | pt l = S.singleton $ p1 l
allPts l = 
    let ((x1, y1), (x2,y2)) = (p1 l, p2 l)
        dx = case compare x1 x2 of
            LT -> 1
            EQ -> 0
            GT -> -1
        dy = case compare y1 y2 of
            LT -> 1
            EQ -> 0
            GT -> -1
    in S.insert (x1, y1) (allPts $ Line {p1=(x1+dx, y1+dy), p2=(x2, y2)})

allIntersections :: Ord a => S.Set a -> [S.Set a] -> S.Set a
allIntersections existing [] = S.empty 
allIntersections existing (curr:rest) =
    let ints = S.intersection curr existing
    in S.union ints $ allIntersections (S.union curr existing) rest
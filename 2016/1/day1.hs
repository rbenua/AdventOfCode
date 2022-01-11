import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit, isAlphaNum )
import qualified Data.Set as S

search :: (Char -> Bool) -> String -> [String]
search pred [] =  []
search pred s =
    let dp = dropWhile (not . pred) s
        (ns, rest) = span pred dp
    in if null ns
    then search pred rest
    else ns : search pred rest

split :: Eq a => a -> [a] -> [[a]]
split sep [] = []
split sep l =
    let (first, rest) = span (/= sep) l
        r = case rest of
            [] -> []
            _:xs -> xs
    in first:split sep r 

repl :: Integral n => n -> (a -> a) -> a -> a
repl 0 _ v = v
repl n f v = repl (n - 1) f (f v)


main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ case args of
        [] -> "input.txt"
        name:_ -> name
    let directions = search isAlphaNum input
    putStr "Part 1: "
    print $ mdist $ snd $ foldl travel (0, (0, 0)) directions
    putStr "Part 2: "
    print $ mdist $ travel2 S.empty (0, (0, 0)) directions
    return ()

deltas :: [(Int, Int)]
deltas = [(0, 1), (1, 0), (0, -1), (-1, 0)]

travel :: (Int, (Int, Int)) -> String -> (Int, (Int, Int))
travel _ [] = error "empty insn"
travel (facing, (x, y)) (dir:amt) =
    let new_facing = case dir of
            'L' -> (facing + 3) `mod` 4 -- haskell has 2 'modulo' functions and neither of them are correct for negative numbers
            'R' -> (facing + 1) `mod` 4
            _ -> error "bad facing"
        (dx, dy) = deltas !! new_facing
        a = read amt
    in (new_facing, (x + dx * a, y + dy * a))

mdist :: Num a => (a, a) -> a
mdist (x, y) = abs x + abs y

go :: (Int, Int) -> Int -> (Int, Int) -> S.Set (Int, Int) -> Either (Int, Int) ((S.Set (Int, Int)), (Int, Int))
go _ 0 p existing =
    if S.member p existing
    then Left p
    else Right (existing, p)
go (dx, dy) amt p@(x, y) existing =
    if S.member p existing
    then Left p
    else go (dx, dy) (amt - 1) (x + dx, y + dy) (S.insert p existing)

travel2 :: S.Set (Int, Int) -> (Int, (Int, Int)) -> [String] -> (Int, Int)
travel2 _ _ [] = error "never repeated a location"
travel2 _ _ ([]:_) = error "empty insn"
travel2 existing (facing, p) ((dir:amt):rest) =
    let new_facing = case dir of
            'L' -> (facing + 3) `mod` 4 -- haskell has 2 'modulo' functions and neither of them are correct for negative numbers
            'R' -> (facing + 1) `mod` 4
            _ -> error "bad facing"
        delta = deltas !! new_facing
        a = read amt
    in case go delta a p existing of
            Left p -> p
            Right (new_pts, np) -> travel2 new_pts (new_facing, np) rest
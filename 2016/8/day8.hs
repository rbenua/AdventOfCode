import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit )
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
    let prog = map parseInsn $ lines input
    let resultSet = foldl step S.empty prog
    putStr "Part 1: "
    print $ S.size resultSet
    putStrLn "Part 2:"
    printGrid2 resultSet 0
    putStrLn ""
    return ()

data Insn = Rect Int Int | RotX Int Int | RotY Int Int deriving Show

parseInsn :: String -> Insn
parseInsn s =
    let [num1, num2] = map read $ search isDigit s
    in if take 4 s == "rect"
    then Rect num1 num2
    else if 'x' `elem` s
    then RotX num1 num2
    else RotY num1 num2

rectSet :: Int -> Int -> S.Set (Int, Int)
rectSet mx my = S.fromList [(x, y) | x <- [0..mx-1], y <- [0..my-1]]

roty row amt p@(x, y) =
    if y == row
    then ((x + amt) `mod` xsize, y)
    else p

rotx col amt p@(x, y) =
    if x == col 
    then (x, (y + amt) `mod` ysize)
    else p

xsize = 50
ysize = 6

step :: S.Set (Int, Int) -> Insn -> S.Set (Int, Int)
step prev (Rect x y) = S.union prev $ rectSet x y
step prev (RotX col amt) = S.map (rotx col amt) prev
step prev (RotY row amt) = S.map (roty row amt) prev

printGrid :: S.Set (Int, Int) -> Int -> IO ()
printGrid _ y | y >= ysize = return ()
printGrid s y = do
    putStrLn $ map (\x -> if (x, y) `S.member` s then '█' else ' ') [0..xsize-1]
    printGrid s (y + 1)

halfChar s y x = 
    case (S.member (x, y) s, S.member (x, y + 1) s) of 
        (True, True) -> '█'
        (True, False) -> '▀'
        (False, True) -> '▄'
        (False, False) -> ' '

printGrid2 :: S.Set (Int, Int) -> Int -> IO ()
printGrid2 _ y | y >= ysize = return ()
printGrid2 s y = do
    putStrLn $ map (halfChar s y) [0..xsize-1]
    printGrid2 s (y + 2)
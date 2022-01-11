import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit, isAlpha )
import Data.List (isInfixOf)

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
    -- note: this approach doesn't work if the beginning of a line is in brackets; my input doesn't have any
    let sets = map (alternate . search isAlpha) $ lines input
    let valids = filter (\(outers, inners) -> any abba outers && not (any abba inners)) sets
    putStr "Part 1: "
    print $ length valids
    putStr "Part 2: "
    print $ length $ filter valid2 sets
    return ()

abba :: Eq a => [a] -> Bool
abba l@(a:b:c:d:_) = (a == d && b == c && a /= b) || abba (tail l)
abba _ = False

alternate :: [a] -> ([a], [a])
alternate (a:b:rest) =
    let (ra, rb) = alternate rest
    in (a:ra, b:rb)
alternate [a] = ([a], [])
alternate [] = ([], [])

valid2 (outers, inners) = 
    let all_abas = concatMap abas outers
    in any (hasBab inners) all_abas

abas :: Eq a => [a] -> [(a, a)]
abas s@(a:b:c:_) | a == c && a /= b = (a,b) : abas (tail s)
abas (_:r) = abas r
abas [] = []

hasBab inners (a, b) =
    let bab = [b, a, b]
    in any (isInfixOf bab) inners
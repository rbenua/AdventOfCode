import System.IO
import System.Environment
import Debug.Trace
import Data.Char ( isDigit, digitToInt )
import Distribution.Utils.MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Lazy as M

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
    let ms = filter ((== "00000") . take 5) $ map (hash input) [0..]
    putStrLn $ "Part 1: " ++ part1 ms
    putStrLn $ "Part 2: " ++ part2 ms
    return ()

hash :: Show a => String -> a -> String
hash prefix n =
    let b = BS.pack $ prefix ++ show n
    in show $ md5 b

part1 :: [[b]] -> [b]
part1 = map (!! 5) . take 8

posVal :: [Char] -> (Int, Char)
posVal m = (digitToInt (m !! 5), m !! 6)

part2 :: [[Char]] -> [Char]
part2 ms =
    let ps = buildMap M.empty $ map traceShowId $ filter (\(pos,_) -> pos < 8) $ map posVal ms
    in take 8 $ map snd $ M.toAscList ps 

buildMap :: Ord k => M.Map k a -> [(k, a)] -> M.Map k a
buildMap _ [] = error "infinite list ran out"
buildMap existing ((p, v):rest) =
    if M.size existing == 8
    then existing
    else buildMap (M.insertWith (\a b -> b) p v existing) rest
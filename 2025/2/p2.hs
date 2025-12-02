import System.IO
import Data.List
import System.Environment (getArgs)
import Data.List.Split
main = do
    args <- getArgs
    contents <- readFile (head args) 
    let ranges = map (splitOn "-") $ splitOn "," contents
        results = map checkRange ranges
    print results
    print $ sum $ concat results

checkNum :: String -> Int -> (Bool, Integer)
checkNum digits divs
  | diglen `mod` divs /= 0 = (False, 10 ^ diglen)
  | repfst > value = (match, repfst)
  | otherwise = (match, bigger)
  where diglen = length digits
        value :: Integer = read digits
        segs = chunksOf (diglen `div` divs) digits 
        segvals :: [Integer] = map read segs
        match = all (== head segs) segs
        repfst :: Integer = read $ concat $ replicate divs $ head segs
        bigseg = head segvals + 1
        bigger = if length (show bigseg) > length (head segs)
                 then 10 ^ diglen
                 else read $ concat $ replicate divs $ show bigseg


checkRange :: [String] -> [Integer]
checkRange [lower, higher] 
  | lowval > highval = []
  | lowval < 10 = checkRange ["10", higher] 
  | or matches = lowval:checkRange [next, higher]
  | otherwise = checkRange [next, higher]
  where lowval :: Integer = read lower
        highval :: Integer = read higher
        diglen = length lower
        (matches, nexts) = unzip $ map (checkNum lower) [2..diglen]
        next = show $ minimum nexts
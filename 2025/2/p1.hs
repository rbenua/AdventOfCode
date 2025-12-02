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

checkRange :: [String] -> [Integer]
checkRange [lower, higher] 
  | lowval > highval = []
  | (lowlen == highlen) && lowodd = []
  | lowodd = checkRange ["1" ++ replicate lowlen '0', higher]
  | lowlowval == lowhighval = lowval:checkRange [show $ lowval + 1 + 10 ^ (toInteger lowlen `div` 2), higher]
  | lowlowval > lowhighval = checkRange [concat $ replicate 2 $ show (lowhighval + 1), higher]
  | otherwise = checkRange [lowhigh ++ lowhigh, higher]
  where lowlen = length lower
        highlen = length higher
        lowval :: Integer = read lower
        highval :: Integer = read higher
        lowodd = lowlen `mod` 2 == 1
        (lowhigh, lowlow) = splitAt (lowlen `div` 2) lower
        lowlowval :: Integer = read lowlow
        lowhighval :: Integer = read lowhigh
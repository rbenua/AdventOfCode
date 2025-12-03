import System.IO
import Data.List
import System.Environment (getArgs)
import Data.List.Split
import Data.Char (digitToInt)
main = do
    args <- getArgs
    contents <- readFile (head args) 
    let banks = lines contents
        results = map (checkBank . map digitToInt) banks
    print $ sum results

checkBank :: [Int] -> Int
checkBank [] = 0
checkBank [a] = 0
checkBank (a:rest) =
    let value = 10 * a + maximum rest
    in max value $ checkBank rest

import System.IO
import Data.List
import System.Environment (getArgs)
import Data.List.Split
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Control.Monad.State

main = do
    args <- getArgs
    contents <- readFile (head args) 
    let banks = lines contents
        results = map (flip evalState Map.empty . checkMemo 0 12 . map digitToInt) banks
    print results
    print $ sum results

checkBank :: Int -> [Int] -> Int
checkBank 0 ls = 0
checkBank n ls | n > length ls = 0
checkBank n (a:ls) =
    let value = a * 10 ^ (n - 1) + checkBank (n - 1) ls
    in max value $ checkBank n ls

checkMemo :: Int -> Int -> [Int] -> State (Map.Map (Int, Int) Int) Int
checkMemo start rem ls = do
    mres <- gets (Map.lookup (start, rem))
    case mres of
        Just res -> return res
        Nothing | rem == 0 -> return 0
        Nothing | rem > length ls -> return 0
        _ -> do
            let a:rest = ls
            current <- checkMemo (start + 1) (rem - 1) rest
            next <- checkMemo (start + 1) rem rest
            let curr_res = a * 10 ^ (rem - 1) + current
                res = max curr_res next
            modify $ Map.insert (start, rem) res
            return res

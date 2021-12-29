import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ head args
    return ()
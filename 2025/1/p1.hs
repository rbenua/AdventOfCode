import System.Directory.Internal.Prelude (getArgs)
import System.IO
import Data.List
main = do
    args <- getArgs
    contents <- readFile (head args) 
    let (result, _) = foldl turn (0, 50) $ lines contents
    print result

turn (count, curr) cmd =
    let dir:amt = cmd
        iamt = read amt
        new = if dir == 'R'
              then (curr + iamt) `mod` 100
              else (curr - iamt) `mod` 100
    in 
        if new == 0
        then (count + 1, new)
        else (count, new)
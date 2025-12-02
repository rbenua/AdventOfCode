import System.Directory.Internal.Prelude (getArgs)
import System.IO
import Data.List
main = do
    args <- getArgs
    contents <- readFile (head args) 
    let (result, _) = foldl' turn (0, 50) $ lines contents
    print result

turn (count, curr) cmd =
    let dir:amt = cmd
        iamt = read amt
        new = if dir == 'R'
              then curr + iamt 
              else curr - iamt
        diff = abs (new `div` 100)
        passes = case (curr, dir, new `mod` 100) of
            (0, 'L', _) -> diff - 1
            (_, 'L', 0) -> diff + 1
            _           -> diff
    in 
        (count + passes, new `mod` 100)
import System.IO
import System.Environment

strip l = [c | c <- l, c /= '+']

main = do args <- getArgs
          input <- readFile $ args !! 0
          print $ sum $ map (read . strip) $ lines input

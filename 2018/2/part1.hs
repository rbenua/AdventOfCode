import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S

freqs :: String -> M.Map Char Int
freqs = M.fromListWith (+) . (flip zip) (repeat 1)

test n d = let vals = S.fromList $ M.elems d
         in if S.member n vals then 1 else 0

cksum fs = let twos = sum $ map (test 2) fs
               threes = sum $ map (test 3) fs
           in twos * threes

main = do args <- getArgs
          input <- readFile $ args !! 0
          print $ cksum $ map freqs $ lines input

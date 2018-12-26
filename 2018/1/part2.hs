import System.IO
import System.Environment
import qualified Data.Set as S

strip l = [c | c <- l, c /= '+']

find :: Int -> S.Set Int -> [Int] -> Maybe Int
find total seen (first:l) = if S.member (first + total) seen 
                            then Just (first + total) 
                            else find (first + total) (S.insert (first + total) seen) l
find total seen []        = Nothing

main = do args <- getArgs
          input <- readFile $ args !! 0
          print $ find 0 S.empty $ cycle $ map (read . strip) $ lines input

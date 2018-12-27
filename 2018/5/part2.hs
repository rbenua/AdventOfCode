import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char

reduce [] = []
reduce (c:rest) = case reduce rest of
                  [] -> [c]
                  (n:r) -> if c /= n && toLower c == toLower n then r else (c:n:r)

reducestrip c = reduce . filter (\x -> x /= '\n' && toLower x /= c)

main = do args <- getArgs
          input <- readFile $ args !! 0
          print $ minimum [length $ reducestrip c input | c <- ['a'..'z']]

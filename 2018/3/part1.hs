import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- pull out every part of a String that can be read in
-- for some Read a and ignore the rest
readOut :: Read a => String -> [a]
readOut "" = []
readOut s = case reads s of
  [] -> readOut $ tail s
  [(x, s')] -> x : readOut s'
  _ -> error "ambiguous parse"
ireadOut :: String -> [Int]
ireadOut = readOut


build (_, xa, ya, xl, yl) = S.fromList [(x,y) | x <- [xa..(xa+xl-1)], y <- [ya..(ya+yl-1)]]

overlap any multi (Just s:rest) = overlap (S.union (build s) any) (S.union (S.intersection (build s) any) multi) rest
overlap any multi (Nothing:rest) = overlap any multi rest
overlap any multi []       = (any, multi)

parse :: String -> Maybe (Int, Int, Int, Int, Int)

parse s = case ireadOut s of
          [id, xs, ys, xl, yl] -> Just (id, xs, ys, xl, yl)
          _ -> Nothing


main = do args <- getArgs
          input <- readFile $ args !! 0
          let (any, multi) = overlap S.empty S.empty $ map parse $ lines input
          print $ S.size multi

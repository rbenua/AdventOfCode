import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S

cmp (a, b) | a == b = 0
           | otherwise = 1

check :: String -> String -> Int
check a b = foldl (+) 0 $ map cmp $ zip a b

search t l = case [r | r <- l, check t r == 1] of
             [r] -> Just r
             _ -> Nothing

find_adj :: [String] -> Maybe (String, String)
find_adj []           = Nothing
find_adj (first:rest) = case search first rest of
                        Just other -> Just (first, other)
                        Nothing -> find_adj rest

main = do args <- getArgs
          input <- readFile $ args !! 0
          print $ find_adj $ lines input

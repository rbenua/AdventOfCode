import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List

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

data Action = NewGuard Int
            | Sleep Int
            | Wake Int

parse :: String -> Action
parse s = case (words s) !! 2 of
          "falls" -> Sleep (ireadOut s !! 4)
          "wakes" -> Wake (ireadOut s !! 4)
          "Guard" -> NewGuard (ireadOut s !! 5)
          _ -> NewGuard 0

bump :: Maybe Int -> Maybe Int
bump (Just i) = Just (i + 1)
bump Nothing = Just 1

buildMap :: M.Map Int (M.Map Int Int) -> Int -> Int -> [Action] -> M.Map Int (M.Map Int Int)
buildMap m cg cs [] = m
buildMap m cg cs (NewGuard g:rest) = buildMap m g cs rest
buildMap m cg cs (Sleep min:rest) = buildMap m cg min rest
buildMap m cg cs (Wake min:rest) = buildMap (M.insert cg (foldl (\im k -> M.alter bump k im) (M.findWithDefault M.empty cg m) [cs..min-1]) m) cg cs rest

main = do args <- getArgs
          input <- readFile $ args !! 0
          let sleepData = buildMap M.empty 0 0 $ map parse $ sort $ lines input
          let (amt, mm, mg) = maximum [(sum d, snd $ maximum [(a, m) | (m, a) <- M.assocs d], g) | (g, d) <- M.assocs sleepData]
          --print sleepData
          print (amt, mm, mg)
          print (mg * mm)

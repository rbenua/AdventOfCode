{- HLINT ignore "Use map with tuple-section" -}
import System.IO
import Data.List
import System.Environment (getArgs)
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Array
import Data.List.Split

main = do
    args <- getArgs
    contents <- readFile (head args) 
    let eqns = map reverse $ transpose $ map (split (dropDelims . dropBlanks $ whenElt (== ' '))) $ lines contents
    print $ sum $ map solve eqns

solve (op:args) =
    let o = if op == "+" then (+) else (*)
    in foldr1 o $ map read args
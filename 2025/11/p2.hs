{- HLINT ignore "Use map with tuple-section" -}
import System.IO
import Data.List
import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Array
import Data.List.Split
import Data.Char
import Data.MemoTrie
import Debug.Trace
import Data.Maybe
import Data.Ord
import Data.Bits

main = do
    args <- getArgs
    contents <- readFile (head args)
    let graph = Map.fromList $ map parseLine $ lines contents
    print $ p2 graph

parseLine :: String -> (String, [String])
parseLine line =
    let [source, dsts] = splitOn ": " line
    in (source, splitOn " " dsts)

paths graph dst p' src | src == dst = 1
paths graph dst p' src | Map.notMember src graph = 0
paths graph dst p' src = sum $ map p' $ graph Map.! src

p2 graph =
    let svrfft = memoFix (paths graph "fft") "svr"
        fftdac = memoFix (paths graph "dac") "fft"
        dacout = memoFix (paths graph "out") "dac"
        svrdac = memoFix (paths graph "dac") "svr"
        dacfft = memoFix (paths graph "fft") "dac"
        fftout = memoFix (paths graph "out") "fft"
    in (svrfft * fftdac * dacout) + (svrdac * dacfft * fftout)
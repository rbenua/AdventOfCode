import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as SQ
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace

readInt = read :: String -> Int

buildScores curr board scores marble goal nplayers =
    let insloc = (curr + 2) `mod` (SQ.length board)
        remloc = (curr - 7) `mod` (SQ.length board)
        player = marble `mod` nplayers
    in if marble == goal + 1 then scores
       else if marble `mod` 23 == 0 then buildScores remloc (SQ.deleteAt remloc board) (SQ.adjust (+(marble + (SQ.index board remloc))) player scores) (marble + 1) goal nplayers
       else  buildScores insloc (SQ.insertAt insloc marble board) scores (marble + 1) goal nplayers
        

go nelves nmarbles = do print $ foldr max 0 $ buildScores 0 (SQ.singleton 0) (SQ.replicate nelves 0) 1 nmarbles nelves

main = do [ne, nm] <- getArgs
          go (readInt ne) (readInt nm)

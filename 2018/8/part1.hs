import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char

data TreeNode = Leaf [Int]
              | Node [TreeNode] [Int] deriving (Show)

readInt :: String -> Int
readInt = read

parse (0:nmeta:rest) = (Leaf (take nmeta rest), (drop nmeta rest))
parse (nchildren:nmeta:rest) = let (cn, r) = foldl (\(cs, l) _ -> let (c, rs) = parse l in (c:cs, rs)) ([], rest) $ take nchildren $ repeat 0
                             in (Node cn (take nmeta r), drop nmeta r)
parse [] = error "can't parse an empty list"
parse _ = error "parse error"

total (Leaf md) = sum md
total (Node cs md) = sum md + sum (map total cs)

go filename = do input <- fst <$> parse <$> map readInt <$> words <$> readFile filename
                 --print $ show input
                 print $ total input

main = head <$> getArgs >>= go

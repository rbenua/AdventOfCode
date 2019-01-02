import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char

data TreeNode = Leaf [Int]
              | Node [TreeNode] [Int] deriving (Show)

data ViewNode = VLeaf Int [Int]
              | VNode Int [ViewNode] [Int] deriving (Show)

readInt :: String -> Int
readInt = read

parse (0:nmeta:rest) = (Leaf (take nmeta rest), (drop nmeta rest))
parse (nchildren:nmeta:rest) = let (cn, r) = foldl (\(cs, l) _ -> let (c, rs) = parse l in (c:cs, rs)) ([], rest) $ take nchildren $ repeat 0
                             in (Node (reverse cn) (take nmeta r), drop nmeta r)
parse _ = error "parse error"

total (Leaf md) = sum md
total (Node cs md) = sum md + sum (map total cs)

value (Leaf md) = sum md
value (Node cs md) = sum $ map (value . (cs!!) . (\x -> x - 1)) $ filter ((>) (length cs + 1)) $ md

vvalue (Leaf md) = VLeaf (sum md) md
vvalue n@(Node cs md) = VNode (value n) (map vvalue cs) md

go filename = do input <- fst <$> parse <$> map readInt <$> words <$> readFile filename
                 --print $ show input
                 print $ value input
                 --print $ vvalue input

main = head <$> getArgs >>= go

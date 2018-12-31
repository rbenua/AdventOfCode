import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char

putInSet v (Just (s, n)) = Just (S.insert v s, n)
putInSet v Nothing       = Just (S.singleton v, 0)

addDep (Just (s, n)) = Just (s, n + 1)
addDep Nothing       = Just (S.empty, 1)

addConstraint (eli, ine, map) (f, s) = (S.insert f eli, S.insert s ine, M.alter (putInSet s) f $ M.alter addDep s map)

complete n map = let deps = fst $ map M.! n
                     newmap = M.delete n $ S.foldl (\m dep -> M.update (\(s, nd) -> Just (s, nd - 1)) dep m) map deps
                     neweli = M.keysSet $ M.filter (\(_, n) -> n == 0) newmap
                 in (newmap, neweli)

toposort eli map = case S.minView eli of
                   Nothing -> []
                   Just (n,rest) -> let (newmap, neweli) = complete n map
                                    in n:toposort (S.union rest neweli) newmap

parse s = (head $ words s !! 1, head $ words s !! 7) 

go filename = do (eli, ine, map) <- (foldl addConstraint (S.empty, S.empty, M.empty)) <$> map parse . lines <$> readFile filename
                 print $ toposort (eli S.\\ ine) map

main = head <$> getArgs >>= go

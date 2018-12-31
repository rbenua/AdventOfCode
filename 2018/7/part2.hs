import System.IO
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace

putInSet v (Just (s, n)) = Just (S.insert v s, n)
putInSet v Nothing       = Just (S.singleton v, 0)

addDep (Just (s, n)) = Just (s, n + 1)
addDep Nothing       = Just (S.empty, 1)

addConstraint (eli, ine, map) (f, s) = (S.insert f eli, S.insert s ine, M.alter (putInSet s) f $ M.alter addDep s map)

complete queue n (map, eli) = let deps = fst $ map M.! n
                                  newmap = M.delete n $ S.foldl (\m dep -> M.update (\(s, nd) -> Just (s, nd - 1)) dep m) map deps
                                  neweli = (M.keysSet $ M.filter (\(_, n) -> n == 0) newmap) S.\\ S.map snd queue
                              in (newmap, S.delete n $ S.union eli neweli)

time c = (ord c) - 4
extime c = (ord c) - 64

process :: (Char -> Int) -> Int -> Int -> S.Set (Int, Char) -> S.Set Char -> M.Map Char (S.Set Char, Int) -> Int
process tf currtime currfree queue eli map = 
    let 
        (dones, notdones) = S.partition (\v -> fst v == currtime) queue 
        (newmap, neweli) = foldr (complete queue) (map, eli) $ S.map snd dones
        avail = currfree + S.size dones
        (todo, resteli) = S.splitAt avail neweli
        newfree = avail - S.size todo
        newqueue = S.union notdones $ S.map (\c -> (currtime + tf c, c)) todo
    in if S.null newqueue then currtime else process tf (S.findMin $ S.map fst newqueue) newfree newqueue resteli newmap
         
parse s = (head $ words s !! 1, head $ words s !! 7) 

go filename = do (eli, ine, map) <- (foldl addConstraint (S.empty, S.empty, M.empty)) <$> map parse . lines <$> readFile filename
                 print $ process time 0 5 S.empty (eli S.\\ ine) map

main = head <$> getArgs >>= go

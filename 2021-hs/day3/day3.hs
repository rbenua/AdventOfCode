import System.IO
import System.Environment
import Data.List
import Text.Printf
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ head args
    let rows = lines input
    let (gamma, epsilon) = gammaEpsilon rows
    putStrLn $ "Part 1: " ++ show (parseBinary gamma * parseBinary epsilon)
    let o2 = search (fst . gammaEpsilon) "" rows
    let co2 = search (snd . gammaEpsilon) "" rows
    printf "o2: %s (%d), co2: %s (%d)\n" o2 (parseBinary o2) co2 (parseBinary co2)
    printf "Part 2: %d\n" $ parseBinary o2 * parseBinary co2

gammaEpsilon :: [String] -> (String, String)
gammaEpsilon rows =
    let cols = transpose rows
        l = length rows + 1
        gamma = map (\c -> if length (filter (== '1') c) >= l `div` 2 then '1' else '0') cols
        epsilon = map (\c -> if c == '1' then '0' else '1') gamma
    in (gamma, epsilon)

parseBinary :: String -> Int 
parseBinary = foldl (\rest c -> 2 * rest + if c == '1' then 1 else 0) 0

search :: ([String] -> String) -> String -> [String] -> String
search sel prefix [] = prefix
search sel prefix [a] = prefix ++ a
search sel prefix rows =
    let f = head $ sel rows
        rest = [r | h:r <- rows, h == f]
    in search sel (prefix ++ [f]) rest
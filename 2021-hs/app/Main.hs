module Main where
import System.IO
import System.Environment
import Day1


main :: IO ()
main = do
    args <- getArgs 
    s <- readFile $ head args
    let problem = init_problem s :: Day1
    print $ part1 problem
    print $ part2 problem
import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ head args
    let nums = map read $ lines input
    putStrLn $ "part 1: " ++ show (countDescending nums)
    putStrLn $ "part 2: " ++ show (countThreeWindows nums)

countDescending :: [Int] -> Int
countDescending [] = 0
countDescending [c] = 0
countDescending (fst:rest@(snd:_)) = (if snd > fst then 1 else 0) + countDescending rest

countThreeWindows :: [Int] -> Int
countThreeWindows x | length x <= 3 = 0
countThreeWindows (fst:rest) = (if rest !! 2 > fst then 1 else 0) + countThreeWindows rest
import System.IO
import System.Environment
import System.Posix.Internals (puts)

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ head args
    let insns = map readInsn $ lines input
    let (f, d) = foldl part1 (0, 0) insns
    putStrLn $ "part 1: " ++ show (f * d)
    let (f2, d2, aim) = foldl part2 (0, 0, 0) insns
    putStrLn $ "part 2: " ++ show (f2 * d2)
    return ()

data Insn =
    Forward Int
  | Down Int
  | Up Int

readInsn :: [Char] -> Insn
readInsn line =
    let [cmd, amount] = words line in
    case cmd of
        "forward" -> Forward $ read amount
        "down" -> Down $ read amount
        "up" -> Up $ read amount

part1 :: (Int, Int) -> Insn -> (Int, Int)
part1 (f, d) (Forward x) = (f + x, d)
part1 (f, d) (Down x) = (f, d + x)
part1 (f, d) (Up x) = (f, d - x)

part2 :: (Int, Int, Int) -> Insn -> (Int, Int, Int)
part2 (f, d, aim) (Forward x) = (f + x, d + (aim * x), aim)
part2 (f, d, aim) (Down x) = (f, d, aim + x)
part2 (f, d, aim) (Up x) = (f, d, aim - x)
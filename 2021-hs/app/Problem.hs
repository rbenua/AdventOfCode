module Problem(Problem(..)) where
    class Problem a where
        init_problem :: String -> a
        part1 :: a -> String
        part2 :: a -> String
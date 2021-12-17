module Day1(Day1(..), init_problem, part1, part2) where

import Problem

data Day1 = Day1 {nums :: [Int]} deriving(Show)

count_descending :: [Int] -> Int
count_descending [] = 0
count_descending [c] = 0
count_descending (fst:rest@(snd:_)) = (if snd > fst then 1 else 0) + count_descending rest

count_three_windows :: [Int] -> Int
count_three_windows x | length x <= 3 = 0
count_three_windows (fst:rest) = (if rest !! 2 > fst then 1 else 0) + count_three_windows rest


instance Problem Day1 where
    init_problem s = Day1{nums=map read $ lines s}
    part1 = show . count_descending . nums
    part2 = show . count_three_windows . nums
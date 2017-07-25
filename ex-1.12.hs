-- 1
-- 11
-- 121
-- 1331
-- 14641

pascal i j
    | i == 0 = 1
    | j == 0 = 1
    | i < j = 1
    | otherwise = pascal (i-1) j + pascal (i-1) (j-1)

testPyramid = [[pascal i j | j <- [0..i+1]] | i <- [0..5]]
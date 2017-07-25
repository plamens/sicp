import Data.Function

sumSqLarger x y z
    | x <= y && x <= z = sumSq y z
    | y <= x && y <= z = sumSq x z
    | otherwise = sumSq x y
    where sumSq = (+) `on` (^2)
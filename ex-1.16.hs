exp' b n = expIter b n 1

expIter :: Int -> Int -> Int -> Int
expIter b n a =
    if n == 0
        then a
        else if even n
            then expIter (b^2) (n `div` 2) a
            else expIter (b^2) (n `div` 2) (a*b)
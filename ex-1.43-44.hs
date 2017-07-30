repeated f n = foldl (.) id (replicate n f)

repeated' f n = if n == 0
    then id
    else f . (repeated' f (n-1))

smooth f x = avg (f (x-0.1)) (f (x+0.1))

avg x y = (x+y) / 2

nFoldSmooth = repeated smooth
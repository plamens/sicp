p = p

test x y = if x == 0
    then 0
    else y

applicativeOrNormalOrder = test 0 p
import Data.Ratio

contFrac :: (Integer -> Rational) -> (Integer -> Rational) -> Integer -> Rational
contFrac fn fd k = contFracIter fn fd (k-1) ((fn k) / (fd k))

contFracIter :: (Integer -> Rational) -> (Integer -> Rational) -> Integer -> Rational -> Rational
contFracIter fn fd i d =
    if i == 0
        then d
        else contFracIter fn fd (i-1) ((fn i) / ((fd i) + d))

oneOverPhi = fromRational $ contFrac (\_ -> 1%1) (\_ -> 1%1) 11
-- k=11 to get 4 decimals accurate

efd i = if (i `rem` 3) == 2
    then (((i+1)`div`3)*2)%1
    else 1%1
eMinusTwo = fromRational $ contFrac (\_ -> 1%1) efd 11
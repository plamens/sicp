f n = if n < 3 then n else fIter 2 1 0 (n-2)

fIter a b c n = if n == 0
    then a
    else fIter (a + 2*b + 3*c) a b (n-1)

fRec n = if n < 3
    then n
    else fRec (n-1) + 2 * fRec (n-2) + 3 * fRec (n-3)
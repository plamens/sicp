mult a b = multIter a b 0

multIter a b c = if a == 1
    then b+c
    else if even a
        then multIter (halve a) (double b) c
        else multIter (halve a) (double b) (c+b)

halve = flip div 2
double = (*2)
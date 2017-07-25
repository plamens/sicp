mult a b = if a == 1
    then b
    else if even a
        then mult (halve a) (double b)
        else b + (mult (halve a) (double b))

halve = flip div 2
double = (*2)
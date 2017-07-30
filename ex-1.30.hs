sum' term a next b =
    let sumIter term a next b acc =
        if a > b
            then acc
            else sumIter (next a) (acc + (term a))
    in sumIter a 0


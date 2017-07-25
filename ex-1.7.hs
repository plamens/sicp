sqrtIter guess x =
    let newGuess = improve guess x
        improve guess x = (guess + x/guess) / 2
        guessChange = abs $ (newGuess - guess) / guess
    in if guessChange < 0.001
        then newGuess
        else sqrtIter newGuess x
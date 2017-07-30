import Control.Monad.Trans.Writer

fixedPoint :: (Fractional a, Ord a, Show a) => (a -> a) -> a -> Writer [String] a
fixedPoint f guess =
    let newGuess = ((f guess) + guess) / 2
        guessChange = abs ((newGuess - guess) / guess)
    in if guessChange < 0.00001
        then do
            tell [show newGuess ++ " is good enough."]
            return newGuess
        else do
            tell ["Trying again with " ++ show newGuess]
            fixedPoint f newGuess

phi = fst . runWriter $ fixedPoint (\x -> 1 + (1/x)) 1

xToTheX = sequence $ map putStrLn $ snd . runWriter $ fixedPoint (\x -> log 1000/log x) 2
-- About 4 times more steps without average damping
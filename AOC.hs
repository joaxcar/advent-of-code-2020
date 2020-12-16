module AOC where

getInputRaw path = do readFile path
getInputLines path = do lines <$> getInputRaw path
getInputInts path = do map (\x -> read x ::Int) <$> getInputLines path
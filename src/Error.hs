module Error where

errorMustBePositiveNumber :: IO ()
errorMustBePositiveNumber = putStrLn "It must be a positive number!"

errorMustBeEvenRounds :: IO ()
errorMustBeEvenRounds = putStrLn "You must play an even number of rounds!"

errorNoPlayers :: IO ()
errorNoPlayers = putStrLn "Insufficient players to play the game!"

errorMustPickPlayer :: IO ()
errorMustPickPlayer = putStrLn "Invalid player! You must select a valid player!"

errorMustPickColor :: IO ()
errorMustPickColor = putStrLn "Invalid color! You must select a valid color!"

errorInvalidPlayers :: IO ()
errorInvalidPlayers = putStrLn "Something went wrong with the players!"

errorInvalidMaster :: IO ()
errorInvalidMaster = putStrLn "Something went wrong with the selecting a master!"

errorInvalidSecret :: IO ()
errorInvalidSecret = putStrLn "Something went wrong with the selecting a secret!"

errorInvalidRounds :: IO ()
errorInvalidRounds = putStrLn "Something went wrong with the selecting a number of rounds!"


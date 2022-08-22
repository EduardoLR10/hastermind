module Error where

errorMustBePositiveNumber :: IO ()
errorMustBePositiveNumber = putStrLn "You must enter a positive number!"

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

errorInPreparation :: IO ()
errorInPreparation = putStrLn "Something went wrong during preparation!"

errorInvalidGuess :: IO ()
errorInvalidGuess = putStrLn "Something went wrong when taking a guess!"

errorInvalidFeedback :: IO ()
errorInvalidFeedback = putStrLn "Something went wrong when providing feedback!"

errorInvalidGameState :: IO ()
errorInvalidGameState = putStrLn "Something impossible happened!"

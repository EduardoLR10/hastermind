{-# LANGUAGE RecordWildCards #-}
module Messages where

import Types
import Data.List
import System.Environment (getProgName)
import Utils

showPlayers :: [Player] -> String
showPlayers [] = ""
showPlayers players = foldl' showName "" players
  where showName names (Player {..}) = names ++ "Player " ++ name ++ "\n"

showCurrentRound :: Game -> IO ()
showCurrentRound game = putStr $ "\n\n| Round: " ++ show (currentRound game) ++ " | "

showRoundAndBoard :: Game -> IO ()
showRoundAndBoard game = showCurrentRound game >> putStrLn (secretInfo ++ board ++ "\n")
  where board = foldMap showGuess $ playHistory game
        secretInfo = showSecretInfo $ secret game

showSecretInfo :: Secret -> String
showSecretInfo (Secret n _) = "The secret has " ++ show n ++ " slots! |\n"

showGuess :: Play -> String
showGuess Play{..} = "| " ++ showList guess ++ spacer ++ showList fdbck ++ " |\n"
  where
    spacer = " || "
    showList :: Show a => [a] -> String
    showList = foldMap (\c -> " " ++ show c)

printWelcome :: IO ()
printWelcome = putStrLn "\nWelcome to Hastermind! The Mastermind game written in Haskell!"

printUserID :: Int -> IO ()
printUserID id = putStr $ "Enter Player " ++ show id ++ ": "

printColorID :: Int -> String -> IO ()
printColorID id which = putStr $ "Enter " ++ which ++ " " ++ show id ++ ": "

printAskRounds :: IO ()
-- printAskRounds = putStr "How many rounds you guys will play?\nRounds: "
printAskRounds = putStr "How many rounds will you play?\nRounds: "

printAddPlayers :: IO ()
printAddPlayers = putStrLn "| Adding players to the Game |"

printHowManyPlayers :: IO ()
printHowManyPlayers = putStrLn "How many players will play? "

printMasterSelection :: IO ()
printMasterSelection = putStrLn "\n| Master Selection |"

printChooseMaster :: IO ()
printChooseMaster = putStr "Enter who will be the Master: "

printSecretSelection :: IO ()
printSecretSelection = putStrLn "\n| Secret Selection |"


printTakeGuess :: Player -> IO ()
printTakeGuess player = putStrLn $ "Player " ++ name player ++ "! It is your round! Take your guess!\nYour options are: (*Enter colors, one at a time)."

printCallForMaster :: Master -> IO ()
printCallForMaster master = putStrLn $ "Master " ++ name master ++ "! You shall pick your secret code!"

printChooseColor :: IO ()
printChooseColor = putStrLn "Available Colors:"

printAskHowManyColors :: IO ()
-- printAskHowManyColors = putStrLn "How many colors the secret will have?"
printAskHowManyColors = putStrLn "How many colors will the secret have?"

printAskMastersFdbck :: Master -> Int -> IO ()
printAskMastersFdbck Player{..} howMany = putStrLn $ "\nMaster " ++ name ++ ", how did the codebreaker do? \nBlack = One token is both correct color, and in right position \nWhite = One token is correct color, but wrong position \nNone = Neither position or color are correct \nYou have " ++ show howMany ++ " slots for feedback! \nEnter them one at a time."

printCurrentGuess :: Guess -> IO ()
printCurrentGuess guess = putStrLn $ "\nHere is the current guess!\n" ++ foldMap display guess ++ "|"
  where display color = "| " ++ prettifyColorString (show color) ++ " "

printEndGame :: IO ()
printEndGame = putStrLn "| END GAME |"

printWinner :: Name -> IO ()
printWinner name = putStrLn $ "| Congrats " ++ name ++ "! You won! |\n"

printOutOfRounds :: IO ()
printOutOfRounds = putStrLn "\n| We reached the maximum of rounds! |\n"

printBreakerWon :: IO ()
printBreakerWon = putStrLn "\n| A codebreaker discovered the secret! |\n"

printSelectedColors :: IO ()
printSelectedColors = putStrLn "Enter the secret code colors! One color at a time.\nBe sure to remember this! Your options are: \n"

printAvailableColors :: IO ()
printAvailableColors = putStrLn $ foldMap display [minBound :: Color ..maxBound] ++ "|"
  where display color = "| " ++ prettifyColorString (show color) ++ " "

printAvailableFeedback :: IO ()
printAvailableFeedback = putStrLn $ foldMap display [minBound :: Feedback ..maxBound] ++ "|"
  where display color = "| " ++ prettifyColorString (show color) ++ " "

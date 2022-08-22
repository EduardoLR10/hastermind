{-# LANGUAGE RecordWildCards #-}
module Messages where

import Types
import Data.List

showPlayers :: [Player] -> String
showPlayers [] = ""
showPlayers players = foldl' showName "" players
  where showName names (Player {..}) = names ++ "Player " ++ show name ++ "\n"

showCurrentRound :: Game -> IO ()
showCurrentRound game = putStrLn $ "\n| Round: " ++ show (currentRound game) ++ " |\n"

printWelcome :: IO ()
printWelcome = putStrLn "Welcome to Hastermind! The mastermind game written in Haskell!"

printAskRounds :: IO ()
printAskRounds = putStr "How many rounds you guys will play?\nRounds: "

printAddPlayers :: IO ()
printAddPlayers = putStrLn "| Adding players to the Game |"

printHowManyPlayers :: IO ()
printHowManyPlayers = putStrLn "How many players will play: "

printMasterSelection :: IO ()
printMasterSelection = putStrLn "| Master Selection |"

printChooseMaster :: IO ()
printChooseMaster = putStrLn "Available Players:"

printSecretSelection :: IO ()
printSecretSelection = putStrLn "| Secret Selection |"

printTakeGuess :: Player -> IO ()
printTakeGuess player = putStrLn $ "Player " ++ show (name player) ++ "! It is your round! Take your guess!"

printCallForMaster :: Master -> IO ()
printCallForMaster master = putStrLn $ "Master " ++ show master ++ "! You shall pick your secret code!"

printChooseColor :: IO ()
printChooseColor = putStrLn "Available Colors:"

printAskHowManyColors :: IO ()
printAskHowManyColors = putStrLn "How many colors the secret will have?"

printAskMastersFdbck :: IO ()
printAskMastersFdbck = putStrLn "Mastermind, how did the codebreaker do? \n \t Black: One token is both correct color, and in right position \n \t White: One token is correct color, but wrong position \n \t None: Neither position or color are correct \n \t You have 4 slots for feedback! \n"

printEndGame :: IO ()
printEndGame = putStrLn "| END GAME |"

printWinner :: Name -> IO ()
printWinner name = putStrLn $ "\n\n| Congrats " ++ name ++ " ! You won! |\n\n"

printOutOfRounds :: IO ()
printOutOfRounds = putStrLn "\n\n| We reached the end of the game! |\n\n"

printAvailableColors :: IO ()
printAvailableColors = putStrLn $ foldMap display [minBound :: Color ..maxBound] ++ "|"
  where display color = "| " ++ show color ++ " "

{-# LANGUAGE RecordWildCards #-}
module Messages where

import Types
import Data.List
import System.Environment (getProgName)

showPlayers :: [Player] -> String
showPlayers [] = ""
showPlayers players = foldl' showName "" players
  where showName names (Player {..}) = names ++ "Player " ++ show name ++ "\n"

showCurrentRound :: Game -> IO ()
showCurrentRound game = putStr $ "\n| Round: " ++ show (currentRound game) ++ " | "

showRoundAndBoard :: Game -> IO ()
showRoundAndBoard game = showCurrentRound game >> putStrLn (secretInfo ++ board ++ "\n")
  where board = foldMap showGuess $ guessesHistory game
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
printWelcome = putStrLn "Welcome to Hastermind! The mastermind game written in Haskell!"

printPreparetionCompleted :: IO ()
printPreparetionCompleted = putStrLn "Game configuration is set!"

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

printAskMastersFdbck :: Int -> IO ()
printAskMastersFdbck howMany = putStrLn $ "\nMastermind, how did the codebreaker do? \nBlack: One token is both correct color, and in right position \nWhite: One token is correct color, but wrong position \nNone: Neither position or color are correct \nYou have " ++ show howMany ++ " slots for feedback! \n"

printEndGame :: IO ()
printEndGame = putStrLn "| END GAME |"

printWinner :: Name -> IO ()
printWinner name = putStrLn $ "| Congrats " ++ name ++ "! You won! |\n"

printOutOfRounds :: IO ()
printOutOfRounds = putStrLn "| We reached the maximum of rounds! |\n"

printBreakerWon :: IO ()
printBreakerWon = putStrLn "| A codebreaker discovered the secret! |\n"

printAvailableColors :: IO ()
printAvailableColors = putStrLn $ foldMap display [minBound :: Color ..maxBound] ++ "|"
  where display color = "| " ++ show color ++ " "

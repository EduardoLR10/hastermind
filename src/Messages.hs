{-# LANGUAGE RecordWildCards #-}
module Messages where

import Types
import Data.List

showPlayers :: [Player] -> String
showPlayers [] = ""
showPlayers players = foldl' showName "" players
  where showName names (Player {..}) = names ++ "Player " ++ show name ++ "\n"

printWelcome :: IO ()
printWelcome = putStrLn "Welcome to Hastermind! The mastermind game written in Haskell!"

printRequestForRounds :: IO ()
printRequestForRounds = putStr "How many rounds you guys will play?\nRounds: "

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

printCallForMaster :: Master -> IO ()
printCallForMaster master = putStrLn $ "Master " ++ show master ++ "! You shall pick your secret code!"

printChooseColor :: IO ()
printChooseColor = putStrLn "Available Colors:"

printAskHowManyColors :: IO ()
printAskHowManyColors = putStrLn "How many colors the secret will have?"

printAskMastersFdbck :: IO ()
printAskMastersFdbck = putStrLn "Mastermind, how did the codebreaker do? \n \t 1: One token is both correct color, and in right position \n \t 2: One token is correct color, but wrong position \n \t 3: No additional tokens need feedback \n"

printEndGame :: IO ()
printEndGame = putStrLn "| END GAME |"

printAvailableColors :: IO ()
printAvailableColors = putStrLn $ foldMap display [minBound :: Color ..maxBound] ++ "|"
  where display color = "| " ++ show color ++ " "

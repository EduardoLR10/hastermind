module Presentation where

import Types

showPlayers :: [Player] -> Maybe String
showPlayers [] = Nothing
showPlayers [x] = Nothing
showPlayers (x:xs) = Just $ "Master is " ++ show (name x) ++ "\n" ++ normalPlayers (zip [1..] xs)
  where normalPlayers ((index, player):xs) = "Player " ++ show index ++ " " ++ name player ++"\n" ++ normalPlayers xs
        normalPlayers [] = ""

printWelcome :: IO ()
printWelcome = putStrLn "Welcome to Hastermind! The mastermind game written in Haskell!"

printNoPlayers :: IO ()
printNoPlayers = putStrLn "Insufficient players to play the game!"

printRequestForRounds :: IO ()
printRequestForRounds = putStr "How many rounds you guys will play?\nRounds: " 
                       

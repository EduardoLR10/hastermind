{-# LANGUAGE RecordWildCards #-}
module Utils where

import Types
import Data.Char
import Data.Time.Clock
import Data.Time.Format
import Data.Foldable
import System.IO

data PlayerInfo = Full | Partial

rotatePlayers :: [Player] -> [Player]
rotatePlayers = drop <> take $ 1

prettifyColorString :: String -> String
prettifyColorString "" = ""
prettifyColorString (s:ss) = s : map toLower ss

saveGame :: Maybe Player -> GameStatus -> Game -> IO ()
saveGame winner status game = do
  time <- getCurrentTime
  let filename = "game" ++ formatTime defaultTimeLocale "%Y-%m-%d--%H:%M:%S" time ++ ".txt"
  handle <- openFile filename WriteMode
  hPutStrLn handle $ gameToString game ++ statusToString status ++ winnerToString winner
  hClose handle

winnerToString :: Maybe Player -> String
winnerToString Nothing = "There was no winner!\n"
winnerToString (Just Player{..}) = "The winner was " ++ name ++ "!\n"

gameToString :: Game -> String
gameToString Game{..} = fold [sRoundsPlayed, sMaster, sSecret, sPlays, sPlayers]
  where sRoundsPlayed = roundsToString (currentRound - 1) ++ "\n\n"
        sMaster = masterToString master ++ "\n\n"
        sSecret = secretToString secret ++ "\n\n"
        sPlays = playsToString playHistory ++ "\n"
        sPlayers = foldMap (playerToString Full) players ++ "\n"

playerToString :: PlayerInfo -> Player -> String
playerToString Partial Player{..} =
  "Player: " ++ name ++ "\n"
playerToString Full Player{..} =
  "Player " ++ name ++ " ended with final score of " ++ show score ++ "\n"

playsToString :: [Play] -> String
playsToString = foldMap playToString
  where playToString Play{..} = fold [playerString player, guessToString guess, feedbackToString fdbck]
        playerString = playerToString Partial
        guessToString g = "Guess: " ++ colorsToString g ++ "\n"
        feedbackToString f = "Master's feedback: " ++ colorsToString f ++ "\n"

secretToString :: Secret -> String
secretToString Secret{..} = "The master chose the secret: " ++ colorsToString secretCode

colorsToString :: Show a => [a] -> String
colorsToString = foldMap (\s -> prettifyColorString (show s) ++ " ")

masterToString :: Master -> String
masterToString Player{..} = "The master for this game was " ++ name ++ " and the master's final score was " ++ show score

roundsToString :: Int -> String
roundsToString r = "The game was played for " ++ show r ++ " rounds!"

statusToString :: GameStatus -> String
statusToString BreakerWin = basicWin "A coderbreaker won!\n"
statusToString OutOfRounds = basicWin "We were out of rounds!\n"
statusToString GuessError = basicErr "We had a problem with a guess!\n"
statusToString FeedbackError = basicErr "We had a problem with a feedback!\n"
statusToString Continue = "The game was just nuked and did the impossible!\n"

basicWin = (++) "Winning Condition: "
basicErr = (++) "Error Condition: "

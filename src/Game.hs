{-# LANGUAGE RecordWildCards #-}
module Game where
import System.Random
import Control.Monad.State
import Control.Monad.Reader
import Types
import Match
import Presentation

checkSecret :: Guess -> ReaderT GameConfig IO Status
checkSecret guess = do
  gameConfig <- ask
  let code = secretCode $ secret gameConfig
  if guess == code
    then return Finished
    else return Continue

getPlayers :: ReaderT GameConfig IO (Maybe [Player])
getPlayers = do
  gameConfig <- ask
  let ps = players gameConfig
  case ps of
    [] -> return Nothing
    _ -> return $ Just ps

play :: StateT Game (ReaderT GameConfig IO) ()
play = undefined

masterMind :: IO ()
masterMind = do
  printWelcome
  let checkPlayers = showPlayers matchPlayers
  maybe printNoPlayers putStrLn checkPlayers
  case checkPlayers of
    Nothing -> return ()
    _ -> do
      printRequestForRounds
      stringRounds <- getLine
      let rounds = read stringRounds
          initialGameState = Game rounds 0 [] Continue
          test = runStateT play initialGameState
      return ()

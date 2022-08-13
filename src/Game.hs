{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game where
import System.Random
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Types
import Match
import Error
import Messages
import Control.Error (MaybeT, hoistMaybe)
import Data.Foldable (for_)
import Text.Read (readMaybe)
import Data.Maybe

checkSecret :: Guess -> ReaderT GameConfig IO Status
checkSecret guess = do
  gameConfig <- ask
  let code = secretCode $ secret gameConfig
  if guess == code
    then return Finished
    else return Continue

askForPlayers :: MaybeT IO [Player]
askForPlayers = do
  liftIO printAddPlayers
  liftIO printHowManyPlayers
  (ps :: Maybe Int) <- liftIO $ readMaybe <$> getLine
  case ps of
    Just n | n > 0 -> do
      playerNames <- liftIO $ traverse (const getLine) [1..n]
      case map makePlayer playerNames of
        [] -> MaybeT (return Nothing)
        players -> MaybeT (return (Just players))
    _ ->  do
      liftIO errorMustBePositiveNumber
      askForPlayers

askForMaster :: [Player] -> MaybeT IO Master
askForMaster players = do
  liftIO printMasterSelection
  liftIO printChooseMaster
  candidate <- liftIO getLine
  if candidate `elem` map name players
    then MaybeT (return (Just candidate))
    else do liftIO errorMustPickPlayer
            askForMaster players

askForSecret :: Master -> MaybeT IO Secret
askForSecret master = do
  liftIO printSecretSelection
  liftIO $ printCallForMaster master
  liftIO printAskHowManyColors
  (howMany :: Maybe Int) <- liftIO $ readMaybe <$> getLine
  case howMany of
    Just n | n > 0 -> do
               colors <- liftIO $ traverse (const askAndCheckColor) [1..n]
               MaybeT (return (Just $ makeSecret colors))
    _ -> do
      liftIO errorMustBePositiveNumber
      askForSecret master
  where
    askAndCheckColor :: IO Color
    askAndCheckColor = do
      (candidate :: Maybe Color) <- readMaybe <$> getLine
      case candidate of
        Nothing -> do errorMustPickColor
                      askAndCheckColor
        Just color -> return color

play :: StateT Game (MaybeT IO) ()
play = undefined

-- masterMind :: IO ()
-- masterMind = do
--   printWelcome
--   let checkPlayers = showPlayers matchPlayers
--   maybe printNoPlayers putStrLn checkPlayers
--   case checkPlayers of
--     Nothing -> return ()
--     _ -> do
--       printRequestForRounds
--       stringRounds <- getLine
--       let rounds = read stringRounds
--           initialGameState = Game rounds 0 [] Continue
--           test = runStateT play initialGameState
--       return ()

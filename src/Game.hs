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

checkSecret :: Guess -> Secret -> Status
checkSecret guess secret = do
  if guess == secretCode secret
    then Finished
    else Continue

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

--askMasterFdbck :: IO [Feedback] 
--askMasterFdbck = do
--    printAskMastersFdbck
--    Fdbck <-  replicateM 4 . liftIO $ getLine 
--      case Fdbck of
--            | 1 -> "!" :: Exclamation
--            | 2 -> "X" :: X
--            | 3 -> " " :: None
--            | _ -> putStrLn "Please select '1', '2', or '3' to give proper feedback"
--                askMasterFdbck
--         return Fdbck
        -- take four getLines from master and put it in a single list of Feedback;

prepare :: StateT Game (MaybeT IO) Status
prepare = do
  players <- liftIO (runMaybeT askForPlayers)
  case players of
    Nothing -> do liftIO errorInvalidPlayers
                  return ErrorInPrep
    Just ps -> do
      master <- liftIO (runMaybeT $ askForMaster ps)
      case master of
        Nothing -> do liftIO errorInvalidMaster
                      return ErrorInPrep
        Just m -> do secret <- liftIO (runMaybeT $ askForSecret m)
                     case secret of
                       Nothing -> do liftIO errorInvalidSecret
                                     return ErrorInPrep
                       Just s -> do game <- get
                                    let newGame = makeGame ps m s 10
                                    put newGame
                                    return Prepared

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

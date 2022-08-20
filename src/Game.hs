{-# LANGUAGE ScopedTypeVariables #-}
module Game where
import System.Random
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Types
import Debug
import Error
import Messages
import Utils
import Control.Error (MaybeT, hoistMaybe)
import Data.Foldable (for_)
import Text.Read (readMaybe)
import Data.Maybe
import Data.List

getStatus :: Guess -> Secret -> GameStatus
getStatus guess secret = do
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

askForRounds :: MaybeT IO Rounds
askForRounds = do
  liftIO printAskRounds
  (howMany :: Maybe Int) <- liftIO $ readMaybe <$> getLine
  case howMany of
    Just n | even n -> do
               hoistMaybe howMany
    _ -> do
      liftIO errorMustBeEvenRounds
      askForRounds

--askMasterFdbck :: IO [Feedback] 
--askMasterFdbck = do
--    printAskMastersFdbck
--    Fdbck <-  replicateM (length secret - 1) . liftIO $ getLine 
--      case Fdbck of
--             1 -> "!" :: Exclamation
--             2 -> "X" :: X
--             3 -> " " :: None
--             _ -> putStrLn "Please select '1', '2', or '3' to give proper feedback"
--                askMasterFdbck
--         return Fdbck
       -- take four getLines from master and put it in a single list of Feedback;
        
prepare :: StateT Game (MaybeT IO) PrepStatus
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
                       Just s -> do
                         rounds <- liftIO (runMaybeT askForRounds)
                         case rounds of
                           Nothing -> do liftIO errorInvalidRounds
                                         return ErrorInPrep
                           Just r -> do game <- get
                                        let newGame = makeGame ps m s r
                                        put newGame
                                        return Prepared

askForGuess :: Player -> Int -> MaybeT IO Guess
askForGuess player secretSize = do
  liftIO $ printTakeGuess player
  liftIO $ traverse (const askAndCheckColor) [1..secretSize]
  where
    askAndCheckColor :: IO Color
    askAndCheckColor = do
      (candidate :: Maybe Color) <- readMaybe <$> getLine
      case candidate of
        Nothing -> do errorMustPickColor
                      askAndCheckColor
        Just color -> return color

advanceGame :: Play -> Game -> Game
advanceGame play game = newGame
  where newGame = game { roundsRemaining = r, currentRound = c, guessesHistory = g, status = s}
        r = roundsRemaining game - 1
        c = currentRound game + 1
        g = guessesHistory game ++ [play]
        s = getStatus (guess play) (secret game)

play :: StateT Game (MaybeT IO) GameStatus
play = do
  game <- get
  case status game of
    Finished -> do
      let guesses = guessesHistory game
          lastGuess = head guesses
          winner = player lastGuess
          winnerName = name winner
      liftIO $ printWinner winnerName
      liftIO printEndGame
      return Finished
    Continue -> do
      liftIO $ putStrLn "Show the current round"
      liftIO $ putStrLn "Show the board"
      let currentPlayer = head $ players game
          howMany = numberSlots $ secret game
      guess <- liftIO $ runMaybeT $ askForGuess currentPlayer howMany
      case guess of
        Nothing -> do
          liftIO errorInvalidGuess
          return Finished
        Just g -> do
          let previousPlay = Play g currentPlayer
              newGame = advanceGame previousPlay game
          liftIO $ putStrLn "Ask master for feedback"
          put newGame
          play
    
hastermind :: StateT Game (MaybeT IO) ()
hastermind = do
  liftIO printWelcome
  prepared <- prepare
  case prepared of
    ErrorInPrep -> do
      liftIO errorInPreparation
      return ()
    Prepared -> do
      initialGame <- get
      liftIO $ runGame play initialGame
      return ()

runGame :: StateT Game (MaybeT IO) a -> Game -> IO (Maybe (a, Game))
runGame play init = runMaybeT $ runStateT play init

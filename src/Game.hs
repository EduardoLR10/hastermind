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
import Data.Monoid
import Data.Foldable

getStatus :: Guess -> Game -> GameStatus
getStatus guess game = do
  if guess == secretCode (secret game)
    then BreakerWin
    else if roundsRemaining game == 0
            then OutOfRounds
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

askForMaster :: [Player] -> MaybeT IO (Master, [Player])
askForMaster players = do
  liftIO printMasterSelection
  liftIO printChooseMaster
  candidateName <- liftIO getLine
  let candidate = find (\(Player name _ _) -> name == candidateName) players
  case candidate of
    Nothing -> do
       liftIO errorMustPickPlayer
       askForMaster players
    Just c -> hoistMaybe $ Just (c {timesMaster = timesMaster c + 1}, filter (/= c) players)

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
        
prepare :: StateT Game (MaybeT IO) PrepStatus
prepare = do
  players <- liftIO (runMaybeT askForPlayers)
  case players of
    Nothing -> do
      liftIO errorInvalidPlayers
      return ErrorInPrep
    Just allPlayers -> do
      roles <- liftIO (runMaybeT $ askForMaster allPlayers)
      case roles of
        Nothing -> do
          liftIO errorInvalidMaster
          return ErrorInPrep
        Just (m, ps) -> do
          secret <- liftIO (runMaybeT $ askForSecret m)
          case secret of
            Nothing -> do
              liftIO errorInvalidSecret
              return ErrorInPrep
            Just s -> do
              rounds <- liftIO (runMaybeT askForRounds)
              case rounds of
                Nothing -> do
                  liftIO errorInvalidRounds
                  return ErrorInPrep
                Just r -> do
                  game <- get
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

askMasterFdbck :: Secret -> MaybeT IO [Feedback] 
askMasterFdbck secret = do
    liftIO printAskMastersFdbck
    let howMany = numberSlots secret
    (maybeFdbcks :: [Maybe Feedback]) <-  liftIO $ replicateM (div howMany 2) (readMaybe <$> getLine)
    let fdbcks = catMaybes maybeFdbcks
    if length fdbcks == howMany
      then hoistMaybe $ Just fdbcks
      else hoistMaybe Nothing

updatePlayerScore :: Player -> [Feedback] -> Player
updatePlayerScore player feedbacks = player { score = currentScore + extraScore}
  where currentScore = score player
        (Sum extraScore) = foldMap (Sum . feedbackToScore) feedbacks

feedbackToScore :: Feedback -> Points
feedbackToScore Black = 5
feedbackToScore White = 2
feedbackToScore None = 0

advanceGame :: Play -> Player -> Game -> Game
advanceGame play player game = newGame
  where newGame = game { roundsRemaining = r, currentRound = c, guessesHistory = g, status = s, players = ps}
        r = roundsRemaining game - 1
        c = currentRound game + 1
        g = guessesHistory game ++ [play]
        s = getStatus (guess play) game
        ps = tail (players game) ++ [player]

play :: StateT Game (MaybeT IO) GameStatus
play = do
  game <- get
  case status game of
    OutOfRounds -> do
      liftIO printOutOfRounds
      let winner = maximum $ players game
          winnerName = name winner
      liftIO $ printWinner winnerName
      liftIO printEndGame
      return OutOfRounds
    BreakerWin -> do
      let guesses = guessesHistory game
          lastGuess = head guesses
          winner = player lastGuess
          winnerName = name winner
      liftIO $ printWinner winnerName
      liftIO printEndGame
      return BreakerWin
    Continue -> do
      liftIO $ showCurrentRound game
      liftIO $ putStrLn "Show the board"
      let currentPlayer = head $ players game
          howMany = numberSlots $ secret game
      guess <- liftIO $ runMaybeT $ askForGuess currentPlayer howMany
      case guess of
        Nothing -> do
          liftIO errorInvalidGuess
          return GuessError
        Just g -> do
          feedback <- liftIO (runMaybeT $ askMasterFdbck (secret game))
          case feedback of
            Nothing -> do
              liftIO errorInvalidFeedback
              return FeedbackError
            Just f -> do
              let previousPlay = Play g f currentPlayer
                  newPlayer = updatePlayerScore currentPlayer f
                  newGame = advanceGame previousPlay newPlayer game
              put newGame
              play
    error -> return error              
    
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
      status <- liftIO $ runGame play initialGame
      return ()

runGame :: StateT Game (MaybeT IO) a -> Game -> IO (Maybe (a, Game))
runGame play init = runMaybeT $ runStateT play init

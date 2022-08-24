{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
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
import Text.Read (readMaybe)
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Foldable
import Data.Char
import System.Environment (getProgName)

getStatus :: Guess -> Game -> GameStatus
getStatus guess game = do
  if guess == secretCode (secret game)
    then BreakerWin
    else if roundsRemaining game == 1
            then OutOfRounds
            else Continue

askForPlayers :: MaybeT IO [Player]
askForPlayers = do
  liftIO printAddPlayers
  liftIO printHowManyPlayers
  (ps :: Maybe Int) <- liftIO $ readMaybe <$> getLine
  case ps of
    Just n | n > 0 -> do
      playerNames <- liftIO $ traverse (\i -> printUserID i >> getLine) [1..n]
      hoistMaybe $ mapM makePlayer playerNames
    _ ->  do
      liftIO errorMustBePositiveNumber
      askForPlayers

askForMaster :: [Player] -> MaybeT IO (Master, [Player])
askForMaster players = do
  liftIO printMasterSelection
  liftIO printChooseMaster
  candidateName <- liftIO getLine
  let candidate = find (\(Player name _) -> name == candidateName) players
  case candidate of
    Nothing -> do
       liftIO errorMustPickPlayer
       askForMaster players
    Just c -> hoistMaybe $ Just (c, filter (/= c) players)

askForSecret :: Master -> MaybeT IO Secret
askForSecret master = do
  liftIO printSecretSelection
  liftIO $ printCallForMaster master
  liftIO printAskHowManyColors
  (howMany :: Maybe Int) <- liftIO $ readMaybe <$> getLine
  case howMany of
    Just n | n > 0 -> do
               liftIO printSelectedColors
               liftIO printAvailableColors
               colors <- liftIO $ traverse (\id -> askAndCheckColor id "Color" errorMustPickColor) [1..n]
               MaybeT (return (Just $ makeSecret colors))
    _ -> do
      liftIO errorMustBePositiveNumber
      askForSecret master

askAndCheckColor :: Read a => Int -> String -> IO () -> IO a
askAndCheckColor identifier which errorMessage = do
   (candidate :: Maybe a) <- readMaybe . map toUpper <$> (printColorID identifier which >> getLine)
   case candidate of
     Nothing -> do errorMessage
                   askAndCheckColor identifier which errorMessage
     Just color -> return color

askForRounds :: MaybeT IO Rounds
askForRounds = do
  liftIO printAskRounds
  (howMany :: Maybe Int) <- liftIO $ readMaybe <$> getLine
  case howMany of
    Just n | even n && n > 0 -> do
               hoistMaybe howMany
    _ -> do
      liftIO errorMustBeEvenRounds
      askForRounds
        
prepare :: MaybeT IO Game
prepare = do
  liftIO printWelcome
  players <- liftIO (runMaybeT askForPlayers)
  case players of
    Nothing -> do
      liftIO errorInvalidPlayers
      hoistMaybe Nothing
    Just allPlayers -> do
      roles <- liftIO (runMaybeT $ askForMaster allPlayers)
      case roles of
        Nothing -> do
          liftIO errorInvalidMaster
          hoistMaybe Nothing
        Just (m, ps) -> do
          secret <- liftIO (runMaybeT $ askForSecret m)
          case secret of
            Nothing -> do
              liftIO errorInvalidSecret
              hoistMaybe Nothing
            Just s -> do
              rounds <- liftIO (runMaybeT askForRounds)
              case rounds of
                Nothing -> do
                  liftIO errorInvalidRounds
                  hoistMaybe Nothing
                Just r -> hoistMaybe (Just $ makeGame ps m s r)

askForGuess :: Player -> Int -> MaybeT IO Guess
askForGuess player secretSize = do
  liftIO $ printTakeGuess player
  liftIO printAvailableColors
  liftIO $ traverse (\id -> askAndCheckColor id "Color" errorMustPickColor) [1..secretSize]

askMasterFdbck :: Master -> Secret -> Guess -> MaybeT IO [Feedback] 
askMasterFdbck master secret guess = do
    let howMany = numberSlots secret
    liftIO $ printCurrentGuess guess
    liftIO $ printAskMastersFdbck master howMany 
    (fdbcks :: [Feedback]) <-  liftIO $ traverse (\id -> askAndCheckColor id "Feedback" errorMustPickFeedback) [1..howMany]
    if length fdbcks == howMany
      then hoistMaybe $ Just fdbcks
      else hoistMaybe Nothing

updatePlayerScore :: Player -> PlayerType -> [Feedback] -> Player
updatePlayerScore player pType feedbacks = player { score = currentScore + extraScore}
  where currentScore = score player
        (Sum extraScore) = foldMap (Sum . feedbackToScore pType) feedbacks

feedbackToScore :: PlayerType -> Feedback -> Points
feedbackToScore CodeBreaker = breakerFeedback
feedbackToScore CodeMaker = makerFeedback

makerFeedback :: Feedback -> Points
makerFeedback BLACK = 0
makerFeedback WHITE = 0
makerFeedback NONE = 2

breakerFeedback :: Feedback -> Points
breakerFeedback BLACK = 7
breakerFeedback WHITE = 5
breakerFeedback NONE = 0

advanceGame :: Play -> Player -> Master -> Game -> Game
advanceGame play player m game = newGame
  where newGame = game { roundsRemaining = r, currentRound = c, playHistory = p, status = s, players = ps, master = m}
        r = roundsRemaining game - 1
        c = currentRound game + 1
        p = playHistory game ++ [play]
        s = getStatus (guess play) game
        ps = tail (players game) ++ [player]

play :: StateT Game (MaybeT IO) (GameStatus, Maybe Player)
play = do
  game <- get
  case status game of
    OutOfRounds -> do
      liftIO printOutOfRounds
      let winner = maximum $ players game
          winnerName = name winner
      liftIO $ printWinner winnerName
      liftIO printEndGame
      return (OutOfRounds, Just winner)
    BreakerWin -> do
      liftIO printBreakerWon
      let plays = playHistory game
          lastGuess = head plays
          winner = player lastGuess
          winnerName = name winner
      liftIO $ printWinner winnerName
      liftIO printEndGame
      return (BreakerWin, Just winner)
    Continue -> do
      liftIO $ showRoundAndBoard game
      let currentPlayer = head $ players game
          howMany = numberSlots $ secret game
      guess <- liftIO $ runMaybeT $ askForGuess currentPlayer howMany
      case guess of
        Nothing -> do
          liftIO errorInvalidGuess
          return (GuessError, Nothing)
        Just g -> do
          feedback <- liftIO (runMaybeT $ askMasterFdbck (master game) (secret game) g)
          case feedback of
            Nothing -> do
              liftIO errorInvalidFeedback
              return (FeedbackError, Nothing)
            Just f -> do
              let !newPlayer = updatePlayerScore currentPlayer CodeBreaker f
                  currentPlay = Play g f newPlayer
                  newMaster = updatePlayerScore (master game) CodeMaker f
                  newGame = advanceGame currentPlay newPlayer newMaster game
              put newGame
              play
    error -> return (error, Nothing)              
    
runGame :: Game -> IO (Maybe ((GameStatus, Maybe Player), Game))
runGame init = runMaybeT $ runStateT play init

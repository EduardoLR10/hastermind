module Types where

data Color = Green | Blue | Red | Yellow | Pink | Brown | Orange | Purple deriving (Show, Eq, Read, Enum, Bounded)

type Guess  = [Color]
type Round  = Int
type Rounds = Int

type Name   = String
type Master = Player
type Points = Int

data Player = Player
  { name        :: !String
  , score       :: !Int
  , timesMaster :: !Int
  } deriving Show

instance Eq Player where
  (Player n1 s1 tM1) == (Player n2 s2 tM2) = n1 == n2 && s1 == s2 && tM1 == tM2

instance Ord Player where
 (Player _ s1 _) <= (Player _ s2 _) = s1 <= s2

makePlayer :: Name -> Player
makePlayer name = Player name 0 0

data GameStatus = BreakerWin | Continue | OutOfRounds | GuessError | FeedbackError

data PrepStatus = Prepared | ErrorInPrep

data Secret = Secret
  { numberSlots :: !Int
  , secretCode  :: ![Color]
  } deriving Show

makeSecret :: [Color] -> Secret
makeSecret colors = Secret (length colors) colors

data Play = Play
  { guess  :: !Guess
  , fdbck  :: ![Feedback]
  , player :: !Player
  }

makeGame :: [Player] -> Master -> Secret -> Rounds -> Game
makeGame p m s r = Game r 1 [] m p s Continue
  
data Game = Game
  { roundsRemaining :: !Int
  , currentRound    :: !Int
  , guessesHistory  :: ![Play]
  , master          :: !Master
  , players         :: ![Player]
  , secret          :: !Secret
  , status          :: !GameStatus
  }
           
data Feedback = 
      Black -- Token is right in color and position
    | White -- Token is right color, wrong position
    | None -- Token is neither right color, nor right position
    deriving (Read, Show)

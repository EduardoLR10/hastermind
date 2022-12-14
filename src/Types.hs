module Types where

data Color = GREEN | BLUE | RED | YELLOW | PINK | BROWN | ORANGE | PURPLE deriving (Show, Eq, Read, Enum, Bounded)

type Guess  = [Color]
type Round  = Int
type Rounds = Int

type Name   = String
type Master = Player
type Points = Int

data PlayerType = CodeBreaker | CodeMaker

data Player = Player
  { name        :: !String
  , score       :: !Int
  } deriving Show

instance Eq Player where
  (Player n1 s1) == (Player n2 s2) = n1 == n2 && s1 == s2

instance Ord Player where
 (Player _ s1) <= (Player _ s2) = s1 <= s2

makePlayer :: Name -> Maybe Player
makePlayer "" = Nothing
makePlayer name = Just $ Player name 0

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
  } deriving Show

makeGame :: [Player] -> Master -> Secret -> Rounds -> Game
makeGame p m s r = Game r 1 [] m p s Continue
  
data Game = Game
  { roundsRemaining :: !Int
  , currentRound    :: !Int
  , playHistory  :: ![Play]
  , master          :: !Master
  , players         :: ![Player]
  , secret          :: !Secret
  , status          :: !GameStatus
  }
           
data Feedback = 
      BLACK -- Token is right in color and position
    | WHITE -- Token is right color, wrong position
    | NONE -- Token is neither right color, nor right position
    deriving (Read, Show, Enum, Bounded)

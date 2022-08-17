module Types where

data Color = Green | Blue | Red | Yellow | Pink | Brown | Orange | Purple | White deriving (Show, Eq, Read)

type Position = Int
type Alert = String
type Guess = [Color]
type Round = Int
type Rounds = Int

type Name = String
type Master = String
data Player = Player
  { name :: !String
  , score :: !Int
  , timesMaster :: !Int
  } deriving Show

makePlayer :: Name -> Player
makePlayer name = Player name 0 0

data Status = Prepared | Finished | Continue | ErrorInPrep

data Secret = Secret
  { numberSlots :: !Int
  , secretCode :: ![Color]
  } deriving Show

makeSecret :: [Color] -> Secret
makeSecret colors = Secret (length colors) colors

data Play = Play
  { guess :: !Guess
   --, fdbck :: !Feedback
  , player :: !Player
  }

makeGame :: [Player] -> Master -> Secret -> Rounds -> Game
makeGame p m s r = Game r 0 [] m p s
  
data Game = Game
  { roundsRemaining :: !Int
  , currentRound :: !Int
  , guessesHistory :: ![Play]
  --, fdbckHistory :: ![Play]
  , master :: !Master
  , players :: ![Player]
  , secret :: !Secret
  }
           
data Feedback = 
      Exclamation {getExclamation :: Alert} -- Token is right color and position
    | X  {getX :: Alert} -- Token is right color, wrong position
    | None {getNone :: Alert} -- Token is neither right color, nor right position
    deriving (Read, Show)

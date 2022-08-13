module Types where

data Color = Green | Blue | Red | Yellow | Pink | Brown | Orange | Purple | White deriving (Show, Eq, Read)

type Position = Int
type Alert = String
type Guess = [Color]

type Name = String
type Master = String
data Player = Player
  { name :: !String
  , score :: !Int
  , timesMaster :: !Int
  } deriving Show

makePlayer :: Name -> Player
makePlayer name = Player name 0 0

data Status = Finished | Continue

data Secret = Secret
  { numberSlots :: !Int
  , secretCode :: ![Color]
  } deriving Show

makeSecret :: [Color] -> Secret
makeSecret colors = Secret (length colors) colors

data Play = Play
  { guess :: !Guess
  , player :: !Player
  }

data GameConfig = GameConfig
 { players :: ![Player]
 , secret :: !Secret
 }
  
data Game = Game
  { roundsRemaning :: !Int
  , currentRound :: !Int
  , guessesHistory :: ![Play]
  , status :: !Status
  }
           
data Feedback = 
      Exclamation {getExclamation :: Alert} -- Token is right color and position
    | X  {getX :: Alert} -- Token is right color, wrong position
    | None {getNone :: Alert} -- Token is neither right color, nor right position
    deriving (Read, Show)

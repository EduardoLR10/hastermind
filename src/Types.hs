module Types where

data Color = Green | Blue | Red | Yellow | Pink | Brown | Orange | Purple | White deriving (Show, Eq, Read)

type Position = Int
type Alert = String
type Guess = [Color]

data Player = Player
  { name :: !String
  , score :: !Int
  , timesMaster :: !Int
  }

data Status = Finished | Continue

data Secret = Secret
  { numberSlots :: !Int
  , secretCode :: ![Color]
  }
 
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

module Board where

type Row = Int 
type Col = Int 

data Color = Green | Blue | Red | Yellow | Pink | Brown deriving (Show, Eq, Read)

type Position = (Row, Col)
data Token = Token Color Position
type Alert = String 

data Fdbck = 
      Exclamation {getExclamation :: Alert} -- Token is right color and position
    | X  {getX :: Alert} -- Token is right color, wrong position
    | Nothing {getNothing :: Alert} -- Token is neither right color, nor right position
    deriving (Read, Show)

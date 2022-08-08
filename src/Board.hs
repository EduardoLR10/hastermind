module Board where

import Control.Monad.State
import System.IO
import System.Random
import System.Exit

type Row = Int 
type Col = Int 
type Alert = String 
type Position = (Row, Col)
type TokenState = [[Token]]
type FdbckState = [[Fdbck]]
type BoardState = (TokenState, FdbckState)


data Color = Green | Blue | Red | Yellow | Pink | Brown deriving (Show, Eq, Read)

data Token = Token Color Position  

data Fdbck = 
      Exclamation {getExclamation :: Alert} -- Token is right color and position
    | X  {getX :: Alert} -- Token is right color, wrong position
    | Nothing {getNothing :: Alert} -- Token is neither right color, nor right position
    deriving (Read, Show)


mkPassword :: Token -> [Token] 
mkPassword = undefined 

mkEmptyBoard :: IO String 
mkEmptyBoard = undefined

mkCbGuess :: IO String 
mkCbGuess = do
    cbGuess1 <- read <$> getLine
        --checkGuess
    cbGuess2 <- read <$> getLine
        --checkGuess
    cbGuess3 <- read <$> getLine
        --checkGuess
    cbGuess4 <- read <$> getLine
        --checkGuess
    
    putStrLn "You've chosen " ++ cbGuess1 " " ++ " " cbGuess2 " " ++ cbGuess3 " " ++ cbGuess4 \n \n 
    putStrLn "Would you like to change anything?  Y or N"
        lockItIn <- read <$> getLine
            | lockItIn == "Y" or "y"
                mkCbGuess 
            | lockItIn == "N" or "n"
                cbGuess = [(cbGuess1, (Row , 0)), (cbGuess2, (Row , 1)), (cbGuess3, (Row , 2)), (cbGuess4, (Row , 3))]
            | otherwise putStrLn "You must select either Y or N"
                mkCbGuess 



checkGuess :: String -> String 
checkGuess = undefined 

evalFdbck :: [Token] -> [Fdbck]
evalFdbck = undefined 

newtype ST = BoardState -> BoardState
    undefined 

playGame :: IO () -> [Fdbck]
playGame = 
    -- Ask for cbGuess - 'cb = codebreaker'
    putStrLn "Please select the first two letters of (4) of the following (6) colors: " /n 
                        "Green | Blue | Red | Yellow | Pink | Brown"
    mkCbGuess
   
    evalFdbck -- determine Fdbck - (up-to four Alert icons)
   
    -- board state transformer here (?)
    -- update and display new board state
    -- check if row = turn is greater than (10)
            -- True = putStrLn "Sorry, you lost this one-- better luck next time!" (Restart Game)
            -- False = playGame, and increment row (+1)


module Main where

import Board

main :: IO ()
main = do 
    putStrLn "Welcome to Hastermind!"
    -- thread delay; (to make single player, thinking we use the random generator to make initial sequence)
    
    putStrLn "Generating Passord..."
    -- thread delay
    mkPassword
    
    putStrLn "Ok, I'm ready-"
    -- thread delay
    mkEmptyBoard

    playGame
    
    return ()
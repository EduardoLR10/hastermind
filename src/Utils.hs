module Utils where

import Types
import Data.Char

rotatePlayers :: [Player] -> [Player]
rotatePlayers = drop <> take $ 1

prettifyColorString :: String -> String
prettifyColorString "" = ""
prettifyColorString (s:ss) = s : map toLower ss

saveGame :: GameStatus -> Game -> IO ()
saveGame = undefined


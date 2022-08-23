module Utils where

import Types

rotatePlayers :: [Player] -> [Player]
rotatePlayers = drop <> take $ 1

saveGame :: GameStatus -> Game -> IO ()
saveGame = undefined


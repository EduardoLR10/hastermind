module Utils where

import Types

rotatePlayers :: [Player] -> [Player]
rotatePlayers = drop <> take $ 1

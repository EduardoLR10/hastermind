module Debug where

import Types
import Data.Maybe

testPlayers :: [Player]
testPlayers = fromJust $ mapM makePlayer ["Justin", "Jared"]

testMaster = fromJust $ makePlayer "Eduardo"

testRounds = 3 :: Int
  
testSecret :: Secret
testSecret = Secret 2 [GREEN, RED]

testGame = makeGame testPlayers testMaster testSecret testRounds

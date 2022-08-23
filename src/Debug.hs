module Debug where

import Types

testPlayers :: [Player]
testPlayers = fmap makePlayer ["Justin", "Jared"]

testMaster = makePlayer "Eduardo"

testRounds = 3 :: Int
  
testSecret :: Secret
testSecret = Secret 2 [GREEN, RED]

testGame = makeGame testPlayers testMaster testSecret testRounds

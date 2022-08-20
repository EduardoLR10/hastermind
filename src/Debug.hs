module Debug where

import Types

testPlayers :: [Player]
testPlayers = fmap makePlayer ["Eduardo", "Justin", "Jared"]

testMaster = "Eduardo"

testRounds = 10 :: Int
  
testSecret :: Secret
testSecret = Secret 5 [Green, Red, Purple, Orange, White]

testGame = makeGame testPlayers testMaster testSecret testRounds

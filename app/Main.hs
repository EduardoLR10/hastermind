module Main where

import Types
import Game
import Messages
import Error
import Debug
import Utils
import Control.Monad.Trans.Maybe

main :: IO ()
main = do
  gameStatus <- runMaybeT prepare
  case gameStatus of
    Nothing ->
      errorInPreparation
    Just game -> do
      postStatus <- runGame game
      case postStatus of
        Nothing ->
          errorDuringGame
        Just ((s, w), g) -> do
          saveGame w s g
          return ()


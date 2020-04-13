module Main(main) where

import Initial
import Figures
import Handle
import Game
import Text


import Graphics.Gloss.Interface.Pure.Game(play)
import System.Random (getStdGen)

main :: IO ()
main = do         
          symbols <- build_alphabet 0
          keys  <- build_keys 0
          g<-getStdGen
          play window background fps ( initialState keys True  (showText symbols) g) render handleKeys update




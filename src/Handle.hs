module Handle
 where

import Initial
import Figures
import Game
import Graphics.Gloss.Interface.Pure.Game

-- | Respond to key events.
handleKeys :: Event -> TetrisGame -> TetrisGame

-- left
handleKeys (EventKey (Char 'a') Down _ _) game = oneStepLeft game{_lCounter=1}
handleKeys (EventKey (Char 'a') Up _ _)   game = game{_lCounter=(-1)}

handleKeys (EventKey (Char 'A') Down _ _) game = oneStepLeft game{_lCounter=1}
handleKeys (EventKey (Char 'A') Up _ _)   game = game{_lCounter=(-1)}


handleKeys (EventKey (SpecialKey KeyLeft) Down m1 m2) game = oneStepLeft game{_lCounter=1}
handleKeys (EventKey (SpecialKey KeyLeft) Up m1 m2)   game = game{_lCounter=(-1)}
      
-- right
handleKeys (EventKey (Char 'd') Down _ _) game = oneStepRight game{_rCounter=1}
handleKeys (EventKey (Char 'd') Up _ _)   game = game{_rCounter=(-1)}

handleKeys (EventKey (Char 'D') Down _ _) game = oneStepRight game{_rCounter=1}
handleKeys (EventKey (Char 'D') Up _ _)   game = game{_rCounter=(-1)}

handleKeys (EventKey (SpecialKey KeyRight) Down m1 m2) game = oneStepRight game{_rCounter=1}
handleKeys (EventKey (SpecialKey KeyRight) Up m1 m2)   game = game{_rCounter=(-1)}
  
-- down
handleKeys (EventKey (Char 's') Down _ _) game = oneStepDown.collision $ game{_dCounter=1}
handleKeys (EventKey (Char 's') Up _ _)   game = game{_dCounter=(-1)}

handleKeys (EventKey (Char 'S') Down _ _) game = oneStepDown.collision $ game{_dCounter=1}
handleKeys (EventKey (Char 'S') Up _ _)   game = game{_dCounter=(-1)}


handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = oneStepDown.collision $ game{_dCounter=1}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _)   game = game{_dCounter=(-1)}

-- up (for fun)
handleKeys (EventKey (Char 'w') Down _ _) game
      |checkInter new_movable (_immovable game) = game
      |otherwise                                = game{_movable = new_movable,_center = new_center }
      where new_movable = map (\((x,y),c) -> ((x,y+squareLen),c)) (_movable game)
            new_center  = (fst._center$game,(snd._center$game) + squareLen)

--rotate left
handleKeys (EventKey (Char 'q') Down _ _) game
      |_ended game || _pause game || _begin game = game
      |checkInter new_movable (_immovable game)  = game
      |otherwise                                 = game{_movable = new_movable}
      where new_movable = rotationLeft (_movable game) (_center game)

handleKeys (EventKey (Char 'Q') Down m1 m2) game = handleKeys (EventKey (Char 'q') Down m1 m2) game

handleKeys (EventKey (SpecialKey KeyUp) Down m1 m2) game = handleKeys (EventKey (Char 'q') Down m1 m2) game

--rotate right
handleKeys (EventKey (Char 'e') Down _ _) game
      |_ended game || _pause game || _begin game = game
      |checkInter new_movable (_immovable game)  = game
      |otherwise                                 = game{_movable = new_movable}
      where new_movable = rotationRight (_movable game) (_center game)

handleKeys (EventKey (Char 'E') Down m1 m2) game = handleKeys (EventKey (Char 'e') Down m1 m2) game

--pause
handleKeys (EventKey (Char 'p') Down _ _) game = game{_pause = not (_pause game)}

--restart
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) game = initialState [] False (_showTxt game) (_g game)


-- Do nothing for all other events.
handleKeys _ game = game

{-# LANGUAGE MultiWayIf #-}

module Game
  where

import Initial
import Figures

import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomR,StdGen)

import Data.Char(digitToInt,isDigit,toLower,chr,isNumber)



-- | Moves
----------------------------------------------------------------------------------------------------

oneStepDown::TetrisGame -> TetrisGame
oneStepDown game
     |_ended game || _pause game || _begin game = game
     |otherwise                                 = game {_movable = new_movable, _center=new_center}
     where  new_movable = map (\((x,y),c) -> ((x,y-squareLen),c)) (_movable game)
            new_center  = (fst._center$game,(snd._center$game)-squareLen) 

oneStepLeft::TetrisGame -> TetrisGame
oneStepLeft game
      |_ended game || _pause game || _begin game = game
      |checkInter new_movable (_immovable game) = game
      |otherwise                                = game{_movable = new_movable,_center = new_center}
      where new_movable = map (\((x,y),c) -> ((x-squareLen,y),c)) (_movable game)
            new_center  = ((fst._center$game) - squareLen,snd._center$game)

oneStepRight::TetrisGame -> TetrisGame
oneStepRight game
      |_ended game || _pause game || _begin game = game
      |checkInter new_movable (_immovable game)  = game
      |otherwise                                 = game{_movable = new_movable,_center = new_center}
      where new_movable = map (\((x,y),c) -> ((x+squareLen,y),c)) (_movable game)
            new_center  = ((fst._center$game) + squareLen,snd._center$game)

rotationLeft::[(Point,Color)]->Point->[(Point,Color)]
rotationLeft points (cx,cy) = map (\((x,y),c) -> ((cy-y+cx,x-cx+cy),c)) points

rotationRight::[(Point,Color)]->Point->[(Point,Color)]
rotationRight points (cx,cy) = map (\((x,y),c) -> ((y-cy+cx,cx-x+cy),c)) points

-- | End of moves
----------------------------------------------------------------------------------------------------



initialState::[Picture]->Bool->(Point->Float->String -> Point -> [Picture])->StdGen->TetrisGame  
initialState keys begin showTxt gen = TetrisGame new_fig []  new_center 0 0 new_g False next (-1) (-1) (-1) showTxt False begin keys
         where ((fig,(cx,cy)),g)   = generate gen
               (next,new_g)        = generate g
               new_center          = (cx,cy-disToNext)
               new_fig             = map (\((x,y),c) -> ((x,y-disToNext),c)) fig


render::TetrisGame->Picture
render game
       |_begin game = pictures $ (color (dark.dark$green) $ rectangleSolid widthf (heightf + ad_heightf)):
                          (translate (-120) 240 $ scale (3/13) (3/13) $ keys !! 0):
                          (translate (-120) 160 $ scale (3/13) (3/13) $ keys !! 1):
                          (translate (-120) 80 $ scale (3/13) (3/13) $ keys !! 2):
                          (translate (-120) 0 $ scale (3/13) (3/13) $ keys !! 3):
                          (translate (-120) (-80) $ scale (60/512) (60/512) $ keys !! 4):
                          (translate (-120) (-185) $ scale (60/570) (60/570) $ keys !! 5):
                           _showTxt game (1/30,1/30) 32 "Rotate" (-60,240) ++
                           _showTxt game (1/30,1/30) 32 "Down" (-60,160) ++
                           _showTxt game (1/30,1/30) 32 "Left" (-60,80) ++
                           _showTxt game (1/30,1/30) 32 "Right" (-60,0) ++
                           _showTxt game (1/30,1/30) 32 "Pause" (-60,(-80)) ++
                           _showTxt game (1/30,1/30) 32 "Begin" (-60,(-160)) ++
                           _showTxt game (1/30,1/30) 32 "or" (-30,(-193)) ++
                           _showTxt game (1/30,1/30) 32 "restart" (-60,(-226))

       |otherwise   = pictures $ (translate 0 (hightY + ad_heightf/2+squareLen/2) $ color (dark.dark$white) $ rectangleSolid widthf ad_heightf ) :
                         map f nextObj ++
                         map f objects1 ++
                         foldl (\z l -> l++z) [] (zipWith g objects2 [(-oy),(-oy)+squareLen..]) ++
                         _showTxt game  (1/30,1/30) 34 (show._score$game)  (leftX ,hightY + disToNext + 3.5*squareLen) ++
                         ended
         where
              g l y = map (\(x,c) -> translate x y $ color c $ rectangleSolid squareLen squareLen) l
              f ((x,y),c) = translate x y $ color c $ rectangleSolid squareLen squareLen
              objects1    = _movable   game
              objects2    = _immovable game
              widthf      = fromIntegral game_width
              heightf     = fromIntegral game_height
              ad_heightf  = fromIntegral additional_height
              (nextObj,_) = _next game
              keys        = _keys game
      
              ended       = if |_ended game -> (translate 0 (-30) $ color (dark.dark$red) $ rectangleSolid (fromIntegral game_width) 190):
                                                    _showTxt game (1/30,1/30) 32 "Game Over" (-130,35) ++
                                                    _showTxt game (1/30,1/30) 34 (show._score$game) (-110,-25) ++
                                                    _showTxt game (1/50,1/50) 24 "Press enter" (-130,-85)
                               |otherwise       -> []




yToOy::Float -> Int
yToOy y = t `div` (round  squareLen) where
          t = round (y+oy)


checkInter::[(Point,Color)] -> [[(Float,Color)]] -> Bool
checkInter [] _ = False
checkInter (p:l) objects = singleInter p objects || checkInter l objects


singleInter::(Point,Color) ->[[(Float,Color)]] -> Bool
singleInter ((x,y),_) objects
            |x<leftX || x>rightX || y<lowY = True
            |otherwise  = elem x $ map (\(f,_) -> f) ((objects ++ (repeat [])) !! cell)
            where cell  = yToOy y

myInsert::[(Point,Color)] ->[[(Float,Color)]] -> [[(Float,Color)]]
myInsert objects1 objects2 = foldl (\l p -> singleInsert p l) objects2 objects1


singleInsert::(Point,Color) ->[[(Float,Color)]] ->[[(Float,Color)]]
singleInsert ((x,y),c) objects = (singleInsert1 objects (x,c) cell)
                     where cell = yToOy  y

singleInsert1::[[(Float,Color)]]->(Float,Color)->Int->[[(Float,Color)]]
singleInsert1 [] x 0     = [[x]]
singleInsert1 (l1:l) x 0 =  (x:l1) : l
singleInsert1 (l1:l) x n = l1:(singleInsert1 l x (n-1))
--singleInsert1 [] x n     = []:(singleInsert1 [] x (n-1))  
singleInsert1 [] x n     = reverse ( [x]:take n (repeat []))


removeLines::[[(Float,Color)]] -> Int -> ([[(Float,Color)]],Int)
removeLines objects score = (removed, case count of
                                           1 -> score + 100
                                           2 -> score + 300
                                           3 -> score + 700
                                           4 -> score + 1500
                                           count -> score)
                where 
                     (removed,count) = foldl func ([],0) (reverse  objects)
                     func    = (\(z,_count) l -> if
                             |length l == numx -> (z,_count + 1) 
                             |otherwise        -> (l:z,_count))

generate::StdGen->(([(Point,Color)],Point),StdGen)
generate g =(new_next,new_g)
         where (idx,new_g)       = randomR (0,figuresCount-1) g
               (movable,(cx,cy)) = figures !! idx
               new_movable       = map (\((x,y),c) -> ((x,y+disToNext),c)) movable
               new_center        = (cx,cy+disToNext)
               new_next          = (new_movable,new_center)

collision::TetrisGame -> TetrisGame
collision game
   |_ended game || _pause game || _begin game   = game
   |checkInter below objects2 = let inserted    = myInsert objects1 objects2
                                    new_center  = (next_cx,next_cy-disToNext)
                                    new_movable = map (\((x,y),c) -> ((x,y-disToNext),c)) next_movable
                                    (new_next,new_g)=generate g
                                    (new_immovable,new_score) = removeLines inserted score
                                    
                                    
                      in if |checkEnd new_immovable -> game{_immovable=new_immovable,_score=new_score,_g=new_g,_ended=True}
                            |otherwise              -> game{_movable=new_movable, _immovable=new_immovable,_center=new_center,_score=new_score,_g=new_g,_next=new_next}
                                
   |otherwise                 = game 
   where below = map (\((x,y),c) -> ((x,y-squareLen),c)) objects1
         objects1 = _movable game
         objects2 = _immovable game
         center   = _center game
         score    = _score game
         step     = _step game
         g        = _g game
         (next_movable,(next_cx,next_cy)) = _next game

         

checkEnd::[[(Float,Color)]] -> Bool
checkEnd objects = length objects >=numy

update ::  Float -> TetrisGame -> TetrisGame
update _ game
        |ended || pause || begin   = game
        |new_step `mod` speed == 0 = oneStepDown.collision $ new_game{_step = new_step}
        |otherwise                 = new_game{_step = new_step}
        where ended     = _ended game
              pause     = _pause game
              begin     = _begin game
              new_step  = _step game + 1
              speed     = if |new_step>340*fps  -> 6
                             |new_step>200*fps  -> 10
                             |new_step>80*fps   -> 12
                             |new_step>40*fps   -> 15
                             |new_step>10*fps   -> 20
                             |new_step>5*fps    -> 30
                             |otherwise         -> 60

              lCounter  = if |_lCounter game == -1    -> -1
                             |otherwise         -> (_lCounter game + 1) `mod` 10

              rCounter  = if |_rCounter game == -1    -> -1
                             |otherwise         -> (_rCounter game + 1) `mod` 10

              dCounter  = if |_dCounter game == -1    -> -1
                             |otherwise         -> (_dCounter game + 1) `mod` 10
              
              gamel     = if |lCounter == 0 -> oneStepLeft game
                             |otherwise     -> game
              
              gamer     = if |rCounter == 0 -> oneStepRight gamel
                             |otherwise     -> gamel

              gamed     = if |dCounter == 0 -> oneStepDown.collision$gamer
                             |otherwise     -> gamer
              
              new_game  = gamed{_lCounter=lCounter,_rCounter=rCounter,_dCounter=dCounter}

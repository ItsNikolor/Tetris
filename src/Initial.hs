module Initial
   where


import Graphics.Gloss.Data.Color
import Graphics.Gloss(Point,Picture)
import System.Random (StdGen)

-- | Configure playground
----------------------------------------------------------------------------------------------------
offset,numx,numy,additional_height,fps :: Int

offset = 200


numx=10    -- number of squares along x
numy=20    -- number of squares along y

squareLen::Float
squareLen=30


disToNext::Float

additional_height = (round squareLen) * 7 
disToNext         = squareLen * 3

background :: Color
background = black

fps=60


data TetrisGame = TetrisGame {_movable :: [(Point,Color)],_immovable::[[(Float,Color)]],_center::Point,_score::Int,_step::Int,_g::StdGen,_pause::Bool,_next::([(Point,Color)],Point),_lCounter,_rCounter,_dCounter::Int,_showTxt::(Point->Float->String -> Point -> [Picture]),_ended,_begin::Bool,_keys::[Picture]}

-- | End
----------------------------------------------------------------------------------------------------

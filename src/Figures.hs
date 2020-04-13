module Figures
    ( figures,figuresCount,hightY,game_width,game_height,
      window,lowY,rightX,leftX,oy
    ) where

import Initial

import Graphics.Gloss(Point,Display(InWindow),Picture)
import Graphics.Gloss.Data.Color
import System.Random (StdGen)

-- | Initialize playground
----------------------------------------------------------------------------------------------------
game_width, game_height::Int
game_width        = (*) numx.round $ squareLen
game_height       = numy * (round  squareLen)

oy,ox::Float
oy =  fromIntegral (div game_height 2) - squareLen / 2 + (fromIntegral (div additional_height 2))
ox =  fromIntegral (div game_width 2) - squareLen / 2


allX,allY::[Float]
allX=take numx [(-ox),(-ox)+squareLen..]
allY=take numy [(-oy),(-oy)+squareLen..]

leftX,rightX,lowY,hightY::Float
leftX  = head allX
rightX = head.reverse $ allX
lowY   = head allY
hightY = head.reverse $ allY




window :: Display
window = InWindow "Tetris" (game_width, game_height+additional_height) (offset, offset)

-- | End of initialization
----------------------------------------------------------------------------------------------------

-- | Figures
----------------------------------------------------------------------------------------------------
posBegin::Int
posBegin=(numx `div` 2) - 2
pos1,pos2,pos3,pos4::Float
[pos1,pos2,pos3,pos4]=take 4 (drop posBegin allX)

color1,color2,color3,color4,color5,color6,color7::Color
color1 = red
color2 = green
color3 = blue
color4 = yellow
color5 = cyan
color6 = magenta
color7 = aquamarine

addColor::Color->Point->(Point,Color)
addColor c p = (p,c)

lower,higher::Float
lower  = hightY+squareLen
higher = hightY+2*squareLen
lineFigure,lFigure,gFigure,squareFigure,tFigure,zFigure,zipFigure ::[(Point,Color)]
lineCenter,lCenter,gCenter,squareCenter,tCenter,zCenter,zipCenter ::Point


lineFigure = map (addColor color1) [(pos1,lower),(pos2,lower),(pos3,lower),(pos4,lower)]
lineCenter = ((pos2+pos3)/2,lower-squareLen/2)

lFigure = map (addColor color2) [(pos2,higher),(pos3,higher),(pos4,higher),(pos2,lower)] 
lCenter = (pos3,higher)

gFigure = map (addColor color3) [(pos2,higher),(pos3,higher),(pos4,higher),(pos4,lower)]
gCenter = (pos3,higher)

squareFigure = map (addColor color4) [(pos2,lower),(pos3,lower),(pos2,higher),(pos3,higher)]
squareCenter = ((pos2+pos3)/2,lower+squareLen/2)

tFigure = map (addColor color5) [(pos2,higher),(pos3,higher),(pos4,higher),(pos3,lower)]
tCenter = (pos3,higher)

zFigure = map (addColor color6) [(pos3,higher),(pos3,lower),(pos4,lower),(pos2,higher)]
zCenter = (pos3,higher)

zipFigure = map (addColor color7) [(pos3,higher),(pos3,lower),(pos4,higher),(pos2,lower)]
zipCenter = (pos3,higher)

figures::[([(Point,Color)],Point)]
figures = [(lineFigure,lineCenter),(lFigure,lCenter),(gFigure,gCenter),(squareFigure,squareCenter),(tFigure,tCenter),(zFigure,zCenter),(zipFigure,zipCenter)]

figuresCount::Int
figuresCount =length figures

-- | End of figures
----------------------------------------------------------------------------------------------------

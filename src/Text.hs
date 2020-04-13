{-# LANGUAGE MultiWayIf #-}

module Text
  where

import Graphics.Gloss(Point,Picture,scale,translate)
import Graphics.Gloss.Data.Bitmap(bitmapOfBMP)
import Codec.BMP(readBMP)
import Data.Char(digitToInt,isDigit,toLower,chr,isNumber)

showText::[Picture]->Point->Float->String -> Point -> [Picture]
showText _ _ _ [] _ = []
showText symbols scale offset (' ':rest) (x,y) = showText symbols scale offset rest (x+offset,y) 
showText symbols (scalex,scaley) offset (cur:rest) (x,y) = (translate x y $ scale scalex scaley $ symbols !! idx) :
                                                         showText symbols (scalex,scaley) offset rest (x+offset,y)
           where idx = if
                          |isNumber cur -> digitToInt cur
                          |otherwise   -> (fromEnum $ toLower cur) - fromEnum 'a' + 10


build_alphabet::Int->IO [Picture]
build_alphabet 36 = return []
build_alphabet x  = do 
               Right symb  <- if |x<10 -> readBMP ("images/digits/"++ (chr (fromEnum '0' + x) : ".bmp"))
                                 |otherwise -> readBMP ("images/letters/"++ (chr (fromEnum 'A' + x-10) : ".bmp"))
               pic <- return $ bitmapOfBMP symb
               ans <- build_alphabet (x+1)
               return (pic:ans)

build_keys::Int->IO [Picture]
build_keys 6 = return []
build_keys x = do
             Right key  <- readBMP ("images/keyboard/"++ ((["up","down","left","right","p","enter"]!!x) ++ ".bmp"))
             pic <- return $ bitmapOfBMP key
             ans <- build_keys (x+1)
             return (pic:ans)

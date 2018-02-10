module Main (main) where

import Control.Monad
import Data.Bits
import DrawLib
import DrawLib.Shapes
import DrawLib.Text
import DrawLib.Spline
import DrawLib.Resampling
import Graphics.Win32 (fW_NORMAL)

petal pt angle = withTransform(translate pt`multiply`rotateAround0 angle`multiply`translate(-20,0))$
	spline AA(rgb 192 255 255) spl where
	spl = [((0, 80), (0, 1)), ((20, 180), (1, 2)), ((40, 200), (1, -1)), ((60, 140), (0, -1)), ((20, 50), (-1, 0)), ((0, 80), (0, 1))]

f = (rgb 255 192 255, rgb 96 96 192, "Times New Roman", 128, fW_NORMAL, True, False)

main = writeBMP"C:\\users\\james\\documents\\Test.bmp"$snd$onNewBitmap(750, 550)$do
	fillRect(rgb 192 128 0) (0, 0, 800, 600)
	mapM_(petal(250, 250)) [0,pi*0.25..2*pi]
	drawFilledEllipse True(rgb 255 192 0) (213,213,287,287)
	textOut"Haskell" (450, 400) f
{-spl = polygon[(150, 50), (50, 150), (200, 60)]
main = writeBMP"C:\\users\\james\\documents\\Test.bmp"$snd$onNewBitmap(400, 300)$do
	filledSpline True(rgb 64 64 255) 7 Rectnagl(ellipse(50,50,130,250))
-}
-- 	filledSpline True(rgb 0 212 0) [((20,20),(1,0)),((40,40),(0,1)),((60,60),(1,0)),((80,80),(0,1)),((20,100),(-1,0)),((20,20),(1,0))]
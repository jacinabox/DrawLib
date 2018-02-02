{-# LANGUAGE Trustworthy, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, ImplicitParams, ForeignFunctionInterface, FlexibleContexts, TypeFamilies, TupleSections #-}
module DrawLib.Convolution (useFilter, convolve, swap, correlate, intensity, blur, horzGradient, vertGradient, laplacian) where

import Data.Bits
import Data.Int
import Data.Word
import Data.Fixed
import Data.Array hiding (index)
import Data.IORef
import Data.Maybe
import Data.List
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Applicative
import qualified Control.Monad.Catch as C
import Control.Monad
import DrawLib
import DrawLib.Functional
import Prelude hiding (replicate)

{-# INLINE useFilter #-}
useFilter filter shift im x y = let
		(kw, kh) = askDims' filter
		(wid, ht) = askDims' im
		halfX = kw `quot` 2
		halfY = kh `quot` 2
		x1 = (x - halfX) `max` 0
		y1 = (y - halfY) `max` 0
		x2 = (x + halfX) `min` wid
		y2 = (y + halfY) `min` ht
		shiftBy = fromIntegral (if shift then 127 else 0)
		sz = if shift then
				fromIntegral$(y2 - y1) * (x2 - x1) * 127
			else
				sum(map(\y -> sum $ map (\x -> enTuple (getP' (x, y) filter))
					[0..x2-x1-1]) [0..y2-y1-1]) `max` 1 in
	shiftBy + sum (map (\y -> sum $ map (\x -> (enTuple (getP' (x, y) filter) - shiftBy) * enTuple (getP' (x1 + x, y1 + y) im)) [0..x2-x1-1]) [0..y2-y1-1]) / sz

-- | Convolution encounters a problem with edges. This implementation handles by summing fewer
-- pixels at the edges (using cutoffs).
convolve :: BMP -> Bool -> BMP -> Draw()
convolve filter shift bmp = function'(\x y -> unTuple$useFilter filter shift bmp x y)

-- | Flip the current image corner to corner.
flipped = do
	(wid, ht) <- askDims
	mapM_ (\y -> mapM_ (\x -> unsafeSwap (x, y) (wid - 1 - x, ht - 1 - y)) [0..wid-1]) [0..ht`div`2-1]

{-# INLINE unsafeSwap #-}
unsafeSwap pt pt2 = do
	clr <- unsafeGetP pt
	clr2 <- unsafeGetP pt2
	unsafeSetP pt clr2
	unsafeSetP pt2 clr

-- | Swaps two pixels.
{-# INLINE swap #-}
swap pt pt2 = do
	clr <- getP pt
	clr2 <- getP pt2
	unsafeSetP pt clr2
	unsafeSetP pt2 clr

correlate :: BMP -> Bool -> BMP -> Draw ()
correlate kernel = convolve (snd$onBitmap kernel flipped)

intensity x = rgb n n n where
	n = round$255 * x

-- | Gaussian blur filter.
blur :: Double -> BMP
blur sigma = snd$onNewBitmap(2 * dist, 2 * dist)$function'$ \x y -> intensity$f(x - dist, y - dist) where
	dist = round (3 * sigma)
	f (x, y) = exp(-0.5 * (fromIntegral x ^ 2 + fromIntegral y ^ 2) / sigma ^ 2) / (2 * pi)

horzGradient = snd$onNewBitmap(5, 5)$function'$ \x _ -> intensity(fromIntegral x / 4)

vertGradient = snd$onNewBitmap(5, 5)$function'$ \_ y -> intensity(fromIntegral y / 4)

laplacian = snd$onNewBitmap(3, 3)$function'$curry(listArray ((0, 0), (2, 2)) [0,-1,0,-1,10,-1,0,-1,0] !)
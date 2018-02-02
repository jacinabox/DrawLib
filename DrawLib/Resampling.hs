{-# LANGUAGE Trustworthy, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, ImplicitParams, ForeignFunctionInterface, FlexibleContexts, TypeFamilies, TupleSections #-}
-- | Operations in this library ignore any extant transform.
module DrawLib.Resampling (resample) where

import Data.Bits
import Data.Int
import Data.Word
import Data.Fixed
import Data.Array hiding (index)
import Data.Maybe
import Data.List
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Applicative
import qualified Control.Monad.Catch as C
import Control.Monad
import Codec.BMP
import DrawLib
import DrawLib.Functional
import Prelude hiding (replicate)

----------------------------------------
-- Bilinear resampling

{-# INLINE weights #-}
weights f (lo, hi) = if floor lo == ceiling hi - 1 then
		f(floor lo)
	else fromRational(recip(hi - lo)) * (fromRational(fromIntegral(ceiling lo) - lo) * f(floor lo)
		+ fromRational(hi - fromIntegral (floor hi)) * f(floor hi)
		+ sum(map f [ceiling lo..floor hi-1]))

{-# INLINE weightsSpecial1 #-}
weightsSpecial1 factor f (lo, hi) = fromRational(recip(fromIntegral factor)) * sum(map f[ceiling lo..floor hi-1])

{-# INLINE weightsSpecial2 #-}
weightsSpecial2 f (lo, _) = f(floor lo)

{-# INLINE weightsSpecial3 #-}
weightsSpecial3 f (lo, hi) = fromRational(recip(hi - lo)) * (fromRational(fromIntegral(ceiling lo) - lo) * f(floor lo)
	+ fromRational(hi - fromIntegral (floor hi)) * f (floor hi)
	+ sum(map f[ceiling lo..floor hi-1]))

-- Averages over the pixels in the given rectangle.
{-# INLINE sample #-}
sample weights im ((loX, loY), (hiX, hiY)) = unTuple$weights(\y -> weights(\x -> enTuple$getP'(x, y) im) (loX, hiX)) (loY, hiY)

rescale2 hi hi2 x = fromIntegral (x * (hi2 + 1)) / fromIntegral (hi + 1)

-- Rescales a coordinate by moving it from the given first rectangle to the proportionate spot in the second rectangle.
rescale (hiX, hiY) (hiX2, hiY2) (x, y) = (rescale2 hiX hiX2 x, rescale2 hiY hiY2 y)

down1 (x, y) = (x - 1, y - 1)

-- | Resamples the given image into the current image.
resample :: BMP -> Draw()
resample im = do
	bnds <- askDims
	if bnds == askDims' im then
		mask 1 (0, 0) im
		else
		let (factor, m) = divMod(fst (askDims' im)) (fst bnds) in
		-- Certain cases have optimizations. Let (x, y) be the dimensions of the input image and (x', y') those of the output image.
		if m == 0 && snd(askDims' im) `mod` snd bnds == 0 then
			-- Case: x = ax', y = by' for integer a, b.
			function'$ \x y->sample(weightsSpecial1 factor) im (rescale(down1 bnds) (down1$askDims' im) (x, y), rescale(down1 bnds) (down1$askDims' im) (x + 1, y + 1))
		else if fst bnds `mod` fst(askDims' im) == 0 && snd bnds `mod` snd(askDims' im) == 0 then
			-- Case: x' = ax, y' = by for integer a, b.
			function'$ \x y->sample weightsSpecial2 im (rescale(down1 bnds) (down1$askDims' im) (x, y), rescale(down1 bnds) (down1$askDims' im) (x + 1, y + 1))
		else if fst bnds < fst(askDims' im) && snd bnds < snd(askDims' im) then
			-- Case x' < x, y' < y.
			function'$ \x y->sample weightsSpecial3 im (rescale(down1 bnds) (down1$askDims' im) (x, y), rescale(down1 bnds) (down1$askDims' im) (x + 1, y + 1))
		else
			-- The general case.
			function'$ \x y->sample weights im (rescale(down1 bnds) (down1$askDims' im) (x, y), rescale(down1 bnds) (down1$askDims' im) (x + 1, y + 1))

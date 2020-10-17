{-# LANGUAGE Trustworthy, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, TupleSections #-}
-- | Operations in this module ignore any extant transform.
module DrawLib.Functional where

import Data.Bits
import Data.Int
import Data.Word
import Data.Fixed
import Data.Array hiding (index)
import Data.Maybe
import Data.List
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative
import qualified Control.Monad.Catch as C
import Control.Monad
import Codec.BMP
import DrawLib
--import Control.CUtils.DataParallel
import Prelude hiding (replicate)

clipRect :: RECT -> RECT -> RECT
clipRect (x1, y1, x2, y2) (x1a, y1a, x2a, y2a) = (x1 `max` x1a, y1 `max` y1a, x2 `min` x2a, y2 `min` y2a)

{-
twoD :: BMP -> Initial(Initial COLORREF)
twoD bitmap = Initial ht$ \y->
	Initial wid$ \x-> getP'(x,y) bitmap where
	(wid,ht) = askDims' bitmap-}

-- | This is a functional for applying a function over every point in an image. Different scanlines
--   can be run in parallel; it supports a scan for individual scanlines.
functionalScan :: RECT -> (COLORREF -> Int32 -> Int32 -> State s COLORREF) -> s -> Draw()
functionalScan rt f init = do
	(wid, ht) <- askDims
	let (x1, y1, x2, y2) = clipRect rt(0, 0, wid, ht)
	when(x1<x2&&y1<y2)$unsafeConcF_(fromIntegral$y2-y1)$ \y -> let y' = fromIntegral y+y1 in
		evalStateT
			(mapM_(\x -> do
				c <- lift(unsafeGetP(x, y'))
				s <- get
				let (c', s') = runState(f c x y') s
				put s'
				lift(unsafeSetP(x, y') c'))
				[x1..x2-1])
			init

{-# INLINE function #-}
-- | Fill using a functional.
function rt f = functionalScan rt(\c x y -> return$!f c x y) ()

{-# INLINE function' #-}
function' f = do
	(wid, ht) <- askDims
	function(0, 0, wid, ht) (const f)

{-# INLINE maxColour #-}
maxColour clr clr2 = rgb(getRValue clr`max`getRValue clr2) (getGValue clr`max`getGValue clr2) (getBValue clr`max`getBValue clr2)

{-# INLINE mask #-}
-- | Fill a region according to the given mask bitmap.
mask :: COLORREF -> POINT -> BMP -> Draw()
mask tt (x, y) bmp =
	let (wid, ht) = askDims' bmp in
	function(x, y, x+wid, y+ht)$ \clr x2 y2->maxColour clr(tint tt(getP'(x2-x, y2-y) bmp))

{-# INLINE mask2 #-}
-- | A generalized mask function takes an explicit blending function in order to blend
--   two pixels.
mask2 :: (COLORREF->COLORREF->COLORREF)
	-> POINT
	-> BMP
	-> Draw()
mask2 blendF (x, y) bmp =
	let (wid, ht) = askDims' bmp in
	function(x, y, x+wid, y+ht)$ \clr x2 y2->blendF clr(getP'(x2-x, y2-y) bmp)

{-# NOINLINE onBitmap #-}
onBitmap :: BMP -> Draw t -> (t, BMP)
onBitmap im draw = onNewBitmap(askDims' im)$mask white(0, 0) im>>draw
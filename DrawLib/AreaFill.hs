{-# LANGUAGE Trustworthy, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, ImplicitParams, ForeignFunctionInterface, FlexibleContexts, TypeFamilies, TupleSections #-}

module DrawLib.AreaFill (Filler, areaFill, simpleDifference, simpleFiller, blendingFiller) where

import Data.Bits
import Data.Int
import Data.Word
import Data.Fixed
import Data.Array hiding (index)
import Data.Maybe
import Data.List
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Applicative
import qualified Control.Monad.Catch as C
import DrawLib
import Prelude hiding (replicate)

loopM :: (Monad m) => (t -> Bool) -> (t -> m t) -> t -> m t
loopM condition f x = if condition x then return x else f x>>=loopM condition f

catMaybes' :: S.Seq(Maybe t) -> S.Seq t
catMaybes' = (fromJust<$>). S.filter isJust

nub' :: (Ord u) => (t -> u) -> S.Seq t -> S.Seq t
nub' projection = rec Set.empty where
	rec previous s = case S.viewl s of
		x S.:< xs -> 
			let x' = projection x in
			if Set.member x' previous then
				rec previous xs
			else
				x S.<| rec(Set.insert x' previous) xs
		S.EmptyL -> S.empty

-- | A filler accepts: a match colour and a current pixel colour.
-- It returns a blend ratio; the fill colour is blended with the current
-- colour with the given ratio. Additionally, if the ratio is anything except one,
-- propagation from that pixel is stopped.
type Filler = COLORREF -> COLORREF -> Double

areaFillImpl :: Filler -> COLORREF -> COLORREF -> (S.Seq POINT, S.Seq POINT) -> Draw()
areaFillImpl f clrFill clrMatch = void.loopM
	(null.fst)
	(\(frontier, used) -> do
		dims <- askDims
		let choices = nub' id$
			frontier>>= \(x, y) -> S.fromList[(pred x, y), (x, pred y), (succ x, y), (x, succ y)]
		let choices2 = S.filter(\pt -> inBoundsImpl dims pt
			&&all(/=pt) frontier
			&&all(/=pt) used) choices
		choices3 <- mapM(\pt -> do
			clr <- unsafeGetP pt
			let x = f clrMatch clr
			blend x pt clrFill
			return$!guard(x>=1)>>return pt)
			choices2
		return(catMaybes' choices3, frontier))

-- | Do an area fill with the given colour.
-- Accepts:
--
-- * fillF - A Filler which controls how filling is done; for solid area fill pass 'simpleFiller'.
--
-- * clr - The colour to fill with.
--
-- * pt - The point at which to start filling; this can be transformed by the active transform.
areaFill :: Filler -> COLORREF -> POINT -> Draw()
areaFill fillF clr pt = do
	transform <- askTransform
	let pt2 = linearMap transform pt
	withIdentityTransform$do
		clrMatch <- getP pt2
		unsafeSetP pt2 clr
		areaFillImpl fillF clr clrMatch(S.singleton pt2, S.empty)

simpleDifference :: COLORREF -> COLORREF -> Double
simpleDifference clr clr2 = (abs(fromIntegral(getRValue clr) - fromIntegral(getRValue clr2))
	+ abs(fromIntegral(getGValue clr) - fromIntegral(getGValue clr2))
	+ abs(fromIntegral(getBValue clr) - fromIntegral(getBValue clr2)))
	/ 765

simpleFiller :: Filler
simpleFiller clrMatch clr = if clrMatch == clr then 1 else 0
blendingFiller :: Double -> Double -> Filler
blendingFiller range1 range2 clrMatch clr = 1 - (simpleDifference clrMatch clr - range1) / (range2 - range1)
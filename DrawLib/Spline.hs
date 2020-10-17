{-# LANGUAGE Trustworthy, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, TupleSections #-}

-- | The method this module uses is to separate the drawing process into drawing the
--  antialiased fringe and drawing the filled interior. The former can be made fast
--  because few points are touched, and the latter can likewise be made fast as
--  no antialiasing/supersampling calculations are necessary.
module DrawLib.Spline (FPoint, DrawType(NoAA, AA), BezierSpline, spline, filledSpline, recoverBezierSplineFromParameters) where

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
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative
import Control.Parallel
import qualified Control.Monad.Catch as C
import Control.Monad
--import Control.Exception.Assert
import Data.VectorSpace
import DrawLib
import DrawLib.Functional
import Prelude hiding (replicate)

guardedGetP' pt bmp = if inBoundsImpl(askDims' bmp) pt then getP' pt bmp else 0

------------------------------------------

threeTupleToList (c, b, c2) = [c, b, c2]
boundingBoxImpl splineDim = (minimum ls, maximum ls) where
	ls = filter(\x -> x == x && x /= 1/0)$concat$threeTupleToList <$> splineDim
boundingBox :: [(FPoint, FPoint, FPoint)] -> (FPoint, FPoint)
boundingBox spline = ((xmn, ymn), (xmx, ymx)) where
	(xmn, xmx) = boundingBoxImpl((\(c, b, c2) -> (fst c, fst b, fst c2)) <$> spline)
	(ymn, ymx) = boundingBoxImpl((\(c, b, c2) -> (snd c, snd b, snd c2)) <$> spline)

------------------------------------------

graph :: (FPoint, FPoint, FPoint)
	-> Float
	-> FPoint
graph (c, b, c2) x = lerp(lerp c b x) (lerp b c2 x) x

speed (c, b, c2) x = magnitude(((2 * x) *^ (c ^+^ c2) ^-^ (4 * x) *^ b) ^-^ 2 *^ c ^+^ 2 *^ b)

convertRatio :: (Real a, Fractional b) => a -> b
convertRatio = fromRational.toRational

factor = 0.05
curveLengthEstimate(c, b, c2) = magnitude(b ^-^ c) + magnitude(c2 ^-^ b)
intensities tup = (graph tup &&& speed tup &&& const est) <$> [est,2*est..1-est] where
	est = factor/curveLengthEstimate tup

pairUp ls = zip ls(tail ls)

blendIn intensity (x, y) clr = do
	addColour(i, i2) (brightness(intensity * (1-f) * (1-f2)) clr)
	addColour(i, succ i2) (brightness(intensity * (1-f) * f2) clr)
	addColour(succ i, i2) (brightness(intensity * f * (1-f2)) clr)
	addColour(succ i, succ i2) (brightness(intensity * f * f2) clr)
	where
	(i, f) = properFraction(convertRatio x)
	(i2, f2) = properFraction(convertRatio y)

firstDerivativeSign ls =
	tail
	$scanl'
	(\bool (x1,x2) -> let sign = signum(x2-x1) in
		if sign==0 then bool else sign>0)
	False
	$pairUp ls

quadrantB :: (Float,Float) -> Bool
-- Decide whether the vector lies within the angles [pi/4,3*pi/4] or within the angles
-- [-3*pi/4,-pi/4] which both result in True.
quadrantB (dx, dy) = (dy>0)==(dy>dx)
data DrawType = NoAA | AA | Filled deriving Eq
drawAAFunction :: DrawType -> Double -> Float -> Bool -> (Bool,Bool) -> (Bool,Bool) -> FPoint -> Int32 -> FPoint -> COLORREF -> Draw()
drawAAFunction NoAA _ _ _ _ _ _ _ = setP.(floor***floor)
drawAAFunction AA factor speed _ _ _ _ _ = blendIn(factor * convertRatio speed)
drawAAFunction Filled _ _ qb (prevSign,sign) (prevSignX, signX) (prevX,prevY) startX = \(x,y) clr ->
	do
	let b0 = qb
	let xf = (if sign||signX then id else pred).(if (b0/=sign)||signX then floor else ceiling)
	let yf = (if sign||not signX then id else succ).(if (b0/=signX)||sign then floor else ceiling)
	let x' = xf x
	let y' = yf y
	b <- inBounds(x',y')
	when(floor y/=floor prevY&&b) (do
		clr' <- getP(x',y')
		let set = clr'==white
		let clr'' = if set then 0 else white
		mapM_(\x''->unsafeSetP(x'',y') clr'') [0..x'])

-- It draws the fringe of a spline if 'aa' is set to 'NoAA', or 'AA'. It draws the filled
-- interior of a spline if 'aa' is set to 'Filled'.
--
-- This works by simple calculus. At every sample a small step is taken according to the
-- differentials calculated for that step. The differentials are continually adjusted according
-- to the second differentials. Sampling is an approximation; to determine how finely the curve
-- should be approximated, the rate of sampling is changed as samples are taken, according to the
-- magnitude of the differentials.
--
-- When it comes to creating a filled shape, a simple algorithm is used to do the fill. This
-- algorithm is defined using a one-bit-per-pixel bitmap. As new scanlines are encountered,
-- all bits in the scanline to the left of the current x-value are flipped. This way scanlines
-- are double-counted whenever the curve changes direction; to handle this, an additional
-- bit-flipping operation is done any time the curve changes direction; for this, it is
-- necessary to keep track of from which direction scanlines were entered, above or below.
-- When the drawing is finished, the bits in the bitmap describe the areas that should
-- be filled with coloured pixels. (Also note that this step of the algorithm is optimized
-- for drawing a filled curve that does not cross itself -- using this algorithm to draw
-- a curve that crosses itself leads to a displeasing result.)
--
-- Define the number of edge crossings a ray from a point 'pt' encounters passing through the
-- figure 'figure'. Whether a point is inside a figure, has to do with the parity of this number
-- (which is the same no matter which ray is chosen).
drawAA :: DrawType
	-> Int32
	-> COLORREF
	-> [(FPoint, FPoint, FPoint)]
	-> Draw()
drawAA aa startX clr parms = mapM_(\((_,  prevsign, prevsignX, (prev, _)), (qb, sign, signX, (pt, (speed, est)))) ->
	drawAAFunction aa(convertRatio est) speed qb (prevsign,sign) (prevsignX, signX) prev startX pt clr)
	$pairUp$zip4
	(quadrantB.snd <$> parms')
	(firstDerivativeSign(floor.snd.fst <$> parms'))
	(firstDerivativeSign(floor.fst.fst <$> parms'))
	parms' where
	parms' =concat$intensities <$> parms

-- When the two vectors point in the same/opposite directions, the location of the knot
-- is underdetermined. In this case place the knot equidistant between the two points.
intersectRaysImpl :: FPoint -> FPoint -> FPoint -> FPoint -> FPoint
intersectRaysImpl x1 v1 x2 v2 = if x /= x then 0.5 *^ (x1 ^+^ x2) else x where
	b = (snd x1 + (fst x2 - fst x1) * snd v1 / fst v1 - snd x2) / (snd v2 - fst v2 * snd v1 / fst v1)
	x = x2 ^+^ b *^ v2
intersectRays x1 v1 x2 v2 = if abs(fst v1) < abs(snd v1) then
		swap(intersectRaysImpl(swap x1) (swap v1) (swap x2) (swap v2))
	else
		intersectRaysImpl x1 v1 x2 v2 where
	swap (x, y) = (y, x)

-- Compute parameters of the spline.
splineCoefficients :: BezierSpline -> [(FPoint, FPoint, FPoint)]
splineCoefficients ls =
	map(\((kv, slope), (kv2, slope2)) -> (kv, intersectRays kv slope kv2 slope2, kv2))
	(pairUp ls)

type BezierSpline = [(FPoint, FPoint)]

transformSpline :: ThreeD -> BezierSpline -> BezierSpline
transformSpline transform knots = (fLinearMap transform *** fLinearMap(fst(factorThreeD transform))) <$> knots

-- | Draw an antialiased spline.
spline :: DrawType -> COLORREF -> BezierSpline -> Draw()
spline aa clr knots = do
	transform <- askTransform
	let knots' = transformSpline transform knots
	let parameters = splineCoefficients knots'
	withIdentityTransform$drawAA aa 0 clr parameters

buffer :: FPoint
buffer = (100, 100)

blendF clr clr2 = blendColour(fromIntegral(getRValue clr2)/255)
	clr

shrinkByOne :: BMP -> BMP
shrinkByOne mask = snd$onNewBitmap dims$function'$ \x y->
	if
	(x==0 || getP'(pred x,y) mask /= 0)
	&& (x==wid-1 || getP'(succ x,y) mask /= 0)
	&& (y==0 || getP'(x,pred y) mask /= 0)
	&& (y==ht-1 || getP'(x,succ y) mask /= 0)
	then
	white
	else
	0
	where
	dims@(wid,ht) = askDims' mask

-- | Draw an antialiased filled spline.

filledSpline aa clr knots@(_:_:_) = do
	dims <- askDims
	transform <- askTransform
	let knots' = transformSpline transform knots
	let parameters = splineCoefficients knots'

	-- This is determining the smallest possible bitmap size to render in.
	-- This calculation uses the fact that a cubic spline is bounded inside
	-- a bounding box that bounds certain points computed from the spline
	-- points.
	let (fx, fy) = (fromIntegral *** fromIntegral) dims
	let (ptMn, ptMx) = ((max 0 *** max 0).(^-^ buffer) *** (min fx *** min fy).(^+^ buffer)) (boundingBox parameters)
	let ptMnI = (floor *** floor) ptMn
	let boxDims = (ceiling *** ceiling) (ptMx ^-^ ptMn)
	let parameters' = (\(c, b, c2) -> (c ^-^ ptMn, b ^-^ ptMn, c2 ^-^ ptMn)) <$> parameters

	-- This renders the filled shape.
	let drawT = if aa then AA else NoAA
	let maskBmp = snd
		$onNewBitmap boxDims
		$do
		drawAA Filled 0 white parameters'
		drawAA drawT 0 white parameters'
	mask2(\clr2 bright->blendColour(fromIntegral(getRValue bright)/255) clr clr2) ptMnI maskBmp
filledSpline _ _ _ = return()

recoverBezierSplineFromParameters :: [(FPoint, FPoint, FPoint)] -> BezierSpline
recoverBezierSplineFromParameters parameters = (\(c, b, _) -> (c, b ^-^ c)) <$> parameters
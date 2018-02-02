{-# LANGUAGE Trustworthy, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, ImplicitParams, ForeignFunctionInterface, FlexibleContexts, TypeFamilies, TupleSections #-}

module DrawLib.Shapes (fillRect,
-- ** Bezier spline formers for basic shapes
ellipse, CapStyle(..), pen, Polygon, polygon, rectangle,
-- ** Drawing wrappers
drawEllipse, drawFilledEllipse, drawPenStroke) where

import Data.Bits
import Data.Int
import Data.Word
import Data.Fixed
import Data.Array hiding (index)
import Data.Maybe
import Data.List
import Data.VectorSpace
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Applicative
import qualified Control.Monad.Catch as C
import Control.Monad
import DrawLib
import DrawLib.Functional
import DrawLib.Spline
import Prelude hiding (replicate)

-- | Filled a rectangle.
fillRect clr rect = function rect(\_ _ _ -> clr)

ellipse :: RECT -> BezierSpline
ellipse (x1, y1, x2, y2) = [((fromIntegral x1 + halfX, fromIntegral y1), (1, 0)),
	((fromIntegral x1 + (1+c) * halfX, fromIntegral y1 + (1-c) * halfY), (halfX, halfY)),
	((fromIntegral x2, fromIntegral y1 + halfY), (0, 1)),
	((fromIntegral x1 + (1+c) * halfX, fromIntegral y1 + (1+c) * halfY), (-halfX, halfY)),
	((fromIntegral x1 + halfX, fromIntegral y2), (-1, 0)),
	((fromIntegral x1 + (1-c) * halfX, fromIntegral y1 + (1+c) * halfY), (-halfX, -halfY)),
	((fromIntegral x1, fromIntegral y1 + halfY), (0, -1)),
	((fromIntegral x1 + (1-c) * halfX, fromIntegral y1 + (1-c) * halfY), (halfX, -halfY)),
	((fromIntegral x1 + halfX, fromIntegral y1), (1, 0))]
	where
	c = 0.7071
	halfX = (fromIntegral x2 - fromIntegral x1) / 2
	halfY = (fromIntegral y2 - fromIntegral y1) / 2

perpendicular :: FPoint -> FPoint
perpendicular (x, y) = (y, -x)

data CapStyle = Circular | Pointed | Square deriving Eq
doOffsets delta points = (\(v, direction) -> (v ^+^ delta *^ normalized(perpendicular direction), direction)) <$> points
createCap :: Float -> CapStyle -> (FPoint, FPoint) -> [(FPoint, FPoint)]
createCap delta Circular (v, direction) = doOffsets delta[(v, direction)]
	++ (v ^+^ delta *^ normalized direction, perpendicular direction)
	: doOffsets delta[(v, direction')]
	where
	direction' = (-1) *^ direction
createCap delta Pointed (v, direction) = [(v', direction),
	(v', diagonal),
	(v ^+^ delta *^ normalized direction, diagonal),
	(v ^+^ delta *^ normalized direction, diagonal'),
	(v'', diagonal'),
	(v'', direction')]
	where
	[(v', _)] = doOffsets delta[(v, direction)]
	[(v'', _)] = doOffsets delta[(v, direction')]
	direction' = (-1) *^ direction
	diagonal = lerp direction(perpendicular direction) 0.5
	diagonal' = lerp direction'(perpendicular direction) 0.5
createCap delta Square (v, direction) = [(v', direction), (v', perpendicular direction), (v'', perpendicular direction), (v'', direction')]
	where
	[(v', _)] = doOffsets delta[(v, direction)]
	[(v'', _)] = doOffsets delta[(v, direction')]
	direction' = (-1) *^ direction

-- | A functional to create a pen stroke with a given thickness.
--   Accepts:
--
--     * thickness - In pixels.
--
--     * capStyle - Can be Circular, Pointed, or Square.
--
--     * points - A bezier spline describing the centreline of the stroke.
--
--   Returns: A bezier spline describing the edge of the stroke.
pen :: Float -> CapStyle -> [(FPoint, FPoint)] -> [(FPoint, FPoint)]
pen _ _ [] = []
pen thickness capStyle points = 
	-- When the endpoints meet to form a closed figure, it omits the caps, otherwise
	-- the parity of certain operations in Spline is interfered with.
	if fst(head points) == fst(last points) then
	doOffsets delta points
	++ doOffsets delta(reverse(second((-1)*^) <$> points))
	else
	doOffsets delta points
	++ cap1
	++ doOffsets delta(reverse(second((-1)*^) <$> points))
	++ cap2 where
	delta = thickness / 2
	cap1 = createCap delta capStyle(last points)
	cap2 = createCap delta capStyle(second((-1)*^) (head points))

type Polygon = [FPoint]
polygon :: Polygon -> BezierSpline
polygon poly = concat(createElbow <$> tails(poly ++ take 2 poly)) where
	createElbow(pt:pt2:pt3:_) = [(pt2, pt2 ^-^ pt), (pt2, pt3 ^-^ pt2)]
	createElbow _ = []

rectangle :: RECT -> Polygon
rectangle (x,y,x2,y2) = (fromIntegral *** fromIntegral) <$> [(x,y),(x2,y),(x2,y2),(x,y2)]

drawEllipse :: DrawType -> COLORREF -> RECT -> Draw()
drawEllipse aa clr = spline aa clr.ellipse
drawFilledEllipse :: Bool -> COLORREF -> RECT -> Draw()
drawFilledEllipse aa clr = filledSpline aa clr.ellipse
-- | A functional for drawing pen strokes. It is recommended to use
--  the 'Square' cap style for closed shapes; otherwise there will be an unfilled
--  area at the first point of the spline (caused by the way that the caps are drawn).
drawPenStroke :: Bool -> COLORREF -> Float -> CapStyle -> BezierSpline -> Draw()
drawPenStroke aa clr thickness capStyle = filledSpline aa clr.pen thickness capStyle

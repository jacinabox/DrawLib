{-# LANGUAGE Unsafe, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, ImplicitParams, ForeignFunctionInterface, FlexibleContexts, TypeFamilies, TupleSections #-}

module DrawLib (module Codec.BMP,
-- ** Reexports of commonly used Win32 types and functions
COLORREF, BMP, POINT, SIZE, RECT, getRValue, getGValue, getBValue, rgb, gdiFlush,
-- ** The Draw monad and primitives
defBackground, white, unsafeAsk, unsafeLiftIO, unwrapDraw, Draw, pack, _onNewBitmap, onNewBitmap, newBitmap, withRECT', inBoundsImpl, inBounds, askDims, unsafeGetP, getP, unsafeSetP, setP, askDims', getP',
-- ** The linear transform
FPoint, ThreeD(ThreeD), askTransform, identity, fLinearMap, linearMap, multiply, withTransform, withIdentityTransform, translate, factorThreeD, rotateAround0, rotateAround, scale,
-- ** Colour mixing
blendColour, blend, addColour, tint, brightness, enTuple, unTuple,
-- ** Drawing in a device context
drawBitmap,
-- ** Unsafe concurrency operator
unsafeConcF_
) where
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.ByteString (copy, index, replicate)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe
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
import Control.Exception (ArrayException(..))
import Control.Monad
import Control.CUtils.Conc
import Control.CUtils.ThreadPool
import Control.Parallel.Strategies
import Codec.BMP
import System.IO.Unsafe
import System.Mem.Weak
import Graphics.Win32
import qualified Graphics.Win32
import Graphics.Win32Extras
import Graphics.Utils.Bitmap
import Control.FinallyHandler
import Prelude hiding (replicate)

defBackground = rgb 245 246 247
white = rgb 255 255 255

type FPoint = (Float, Float)
-- An augmented matrix of the form:
--  a1 a2 a3
--  b1 b2 b3
--  0  0  1
data ThreeD = ThreeD {-# UNPACK #-} !Float !Float !Float !Float !Float !Float deriving (Read, Show)

-- | A drawing monad. This monad permits a bitmap to be obtained as a value.
-- The 'Draw' monad is intended to be presented as an 'ErrorT SomeException(State BMP)' functor.
newtype Draw t = Draw{ unDraw :: ReaderT(SIZE, ThreeD, Ptr CChar, HDC) IO t } deriving (Functor, Applicative, Monad)
unsafeAsk = Draw  ask
unsafeLiftIO :: IO t -> Draw t
unsafeLiftIO = Draw. lift

unwrapDraw pr d = runReaderT(unDraw d) pr
instance C.MonadCatch Draw where
	catch d f = unsafeAsk>>= \pr->unsafeLiftIO(C.catch(unwrapDraw pr d) (unwrapDraw pr.f))
instance C.MonadThrow Draw where
	throwM = unsafeLiftIO. C.throwM
instance C.MonadMask Draw where
	mask f = unsafeAsk>>= \pr->unsafeLiftIO$C.mask(\g -> unwrapDraw pr(f(unsafeLiftIO.g.unwrapDraw pr)))
	uninterruptibleMask f = unsafeAsk>>= \pr->
		unsafeLiftIO$C.uninterruptibleMask(\g -> unwrapDraw pr(f(unsafeLiftIO.g.unwrapDraw pr)))

bi bmp = case bmpBitmapInfo bmp of
	InfoV3 b -> b
	InfoV4 b -> dib4InfoV3 b

foreign import stdcall "windows.h CreateDIBSection"
	c_CreateDIBSection :: HDC -> LPBITMAPINFO -> UINT -> Ptr(Ptr CChar) -> HANDLE -> DWORD -> IO HBITMAP
foreign import stdcall "windows.h GdiFlush" gdiFlush :: IO Bool

{-# NOINLINE _onNewBitmap #-}
_onNewBitmap :: SIZE -> Draw t -> IO(t, BMP, HBITMAP, HBITMAP, HDC)
-- Create finally handler for things that hang around with the bitmap (HBITMAP and HDC handles).
_onNewBitmap pr@(wd, ht) draw = newFinallyHandlerResetter>>= \(r, resetR)->
	-- Create finally handler for temporaries.
	newFinallyHandler>>= \rTemps->
	finally' r$finally' rTemps$do
	let len = nBytes wd2 ht2
	-- Prepare device contexts.
	dc <- getDC Nothing
	addFinallyHandler rTemps(releaseDC Nothing dc)
	cdc <- createCompatibleDC(return dc)
	addFinallyHandler r(deleteDC cdc)
	-- Construct a DIB section and select into the device context.
	pp <- malloc
	addFinallyHandler rTemps(free pp)
	let bi = bmpBitmapInfo(pack wd2 ht2 B.empty)
	bmp <- withBITMAP bi(\pbmp -> c_CreateDIBSection dc pbmp dIB_RGB_COLORS pp nullPtr 0)
	when(bmp==nullPtr)$error"onNewBitmap: could not create DIB section"
	addFinallyHandler r(deleteBitmap bmp)
	oldBmp <- selectBitmap cdc bmp
	addFinallyHandler r(selectBitmap cdc oldBmp)
	-- Then create a bytestring.
	p <- peek pp
	fillBytes p 0(nBytes wd ht)
	bs <- unsafePackCStringLen(p, len)
	-- Do drawing....
	x <- runReaderT(unDraw draw) (pr, identity, p, cdc)
	resetR -- Disable finally handler so that 'cdc' is not deleted straight away.
	-- Construct an image value.
	return(x, pack wd2 ht2 bs, oldBmp, bmp, cdc) where
	wd2 = fromIntegral wd
	ht2 = fromIntegral ht

{-# NOINLINE onNewBitmap #-}
onNewBitmap dims draw = unsafePerformIO$do
	(x, bmp, hOldBmp, hbitmap, cdc) <- _onNewBitmap dims draw
	mkWeak bmp bmp$return$do
		selectBitmap cdc hOldBmp
		deleteBitmap hbitmap
		deleteDC cdc
	return(x, bmp)

{-# INLINE newBitmap #-}
newBitmap dim = snd$onNewBitmap dim(return ())

withRECT' :: RECT -> (Ptr RECT -> Draw a) -> Draw a
withRECT' rect f = do
	x <- unsafeAsk
	unsafeLiftIO$withRECT rect((`runReaderT` x).unDraw.f)

{-# INLINE askDims' #-}
askDims' :: BMP -> SIZE
askDims' im = (wd2, ht2) where
	(wd, ht) = bmpDimensions im
	wd2 = fromIntegral wd
	ht2 = fromIntegral ht

{-# INLINE getP' #-}
getP' :: POINT -> BMP -> COLORREF
getP' (x, y) im = rgb(index bs off) (index bs(off+1)) (index bs(off+2)) where
	(wd, ht) = askDims' im
	off = fromIntegral(calculateByteOffset wd ht x y)
	bs = bmpRawImageData im

fstOfFour(x, _, _, _) = x
-- | Retrieve the bitmap's dimensions.
{-# INLINE askDims #-}
askDims :: Draw SIZE
askDims = liftM fstOfFour unsafeAsk

{-# INLINE inBoundsImpl #-}
inBoundsImpl (wid, ht) (x, y) = inRange(0, wid - 1) x && inRange(0, ht - 1) y
{-# INLINE inBounds #-}
inBounds pt = do
	dims <- askDims
	return$!inBoundsImpl dims pt

-- | Note that transformation matrices are not applied for unsafe get/set.
{-# INLINE unsafeGetP #-}
unsafeGetP :: POINT -> Draw COLORREF
unsafeGetP (x, y) = do
	((wd, ht), _, im, _) <- unsafeAsk
	let off = fromIntegral(calculateByteOffset wd ht x y)
	unsafeLiftIO$liftM3 rgb
		(peekByteOff im off)
		(peekByteOff im (off+1))
		(peekByteOff im (off+2))

{-# INLINE getP #-}
getP pt = do
	transform <- askTransform
	let pt2@(x2, y2) = linearMap transform pt
	b <- inBounds pt2
	unless b$do
		(wid, ht) <- askDims
		C.throwM$IndexOutOfBounds$"FRP.Draw.getPixel: " ++ show (x2, y2) ++ " not in " ++ show wid ++ "x" ++ show ht ++ " bitmap"
	unsafeGetP pt2

makeLPARAM x y = shiftL y 16 .|. x

{-# INLINE unsafeSetP #-}
unsafeSetP :: POINT -> COLORREF -> Draw()
unsafeSetP (x, y) clr = do
	((wd, ht), _, im, _) <- unsafeAsk
	let off = fromIntegral(calculateByteOffset wd ht x y)
	unsafeLiftIO$do
		pokeByteOff im off(getRValue clr)
		pokeByteOff im (off+1) (getGValue clr)
		pokeByteOff im (off+2) (getBValue clr)

{-# INLINE setP #-}
setP pt clr = do
	transform <- askTransform
	let pt2 = linearMap transform pt
	b <- inBounds pt2
	when b$unsafeSetP pt2 clr

sndOfFour(_, x, _, _) = x
{-# INLINE askTransform #-}
askTransform :: Draw ThreeD
askTransform = liftM sndOfFour unsafeAsk

{-# INLINE identity #-}
identity = ThreeD 1 0 0 0 1 0
{-# INLINE fLinearMap #-}
fLinearMap :: ThreeD -> FPoint -> FPoint
fLinearMap (ThreeD a1 a2 a3 b1 b2 b3) (x, y) = (a1 * x + a2 * y + a3, b1 * x + b2 * y + b3)
{-# INLINE linearMap #-}
linearMap :: ThreeD -> POINT -> POINT
linearMap transform = (round *** round).fLinearMap transform.(fromIntegral *** fromIntegral)
{-# INLINE multiply #-}
multiply :: ThreeD -> ThreeD -> ThreeD
multiply (ThreeD a1 a2 a3 b1 b2 b3) (ThreeD a1' a2' a3' b1' b2' b3') = ThreeD
	(a1 * a1' + a2 * b1') (a1 * a2' + a2 * b2') (a1 * a3' + a2 * b3' + a3)
	(b1 * a1' + b2 * b1') (b1 * a2' + b2 * b2') (b1 * a3' + b2 * b3' + b3)

-- | Temporarily applies a transform inside the block.
{-# INLINE withTransform #-}
withTransform :: ThreeD -> Draw t -> Draw t
withTransform transform dr = do
	(dims, transform2, p, cdc) <- unsafeAsk
	tr3 <- return$!multiply transform transform2
	unsafeLiftIO(runReaderT(unDraw dr) (dims, tr3, p, cdc))

{-# INLINE withIdentityTransform #-}
--- | Temporarily suspends all transforms within the block.
withIdentityTransform :: Draw t -> Draw t
withIdentityTransform dr = do
	(dims, _, p, cdc) <- unsafeAsk
	unsafeLiftIO(runReaderT(unDraw dr) (dims, identity, p, cdc))

translate :: FPoint -> ThreeD
translate(x, y) = ThreeD 1 0 x 0 1 y
-- | The augmented matrix factors as a 2D transform followed by a translation.
factorThreeD :: ThreeD -> (ThreeD, ThreeD)
factorThreeD (ThreeD a1 a2 a3 b1 b2 b3) = (ThreeD a1 a2 0 b1 b2 0, ThreeD 1 0 a3 0 1 b3)
rotateAround0 :: Float -> ThreeD
rotateAround0 angle = ThreeD(cos angle) (-sin angle) 0 (sin angle) (cos angle) 0
rotateAround :: Float -> FPoint -> ThreeD
rotateAround angle pt = translate pt `multiply` rotateAround0 angle `multiply` translate((-1) *^ pt)
scale (cx, cy) = ThreeD cx 0 0 0 cy 0

blendColour :: Double -> COLORREF -> COLORREF -> COLORREF
blendColour intensity clr clr2 = rgb(formula getRValue) (formula getGValue) (formula getBValue)
	where
	formula f = round((1-intensity)*fromIntegral(f clr2)+intensity*fromIntegral(f clr))

-- | Blend a colour into a pixel.
{-# INLINE blend #-}
blend :: Double -> POINT -> COLORREF -> Draw()
blend intensity pt clr = do
	transform <- askTransform
	let pt2 = linearMap transform pt
	b <- inBounds pt2
	intensity <- return$!(intensity `min` 1) `max` 0
	when b$do
		clr2 <- unsafeGetP pt2
		unsafeSetP pt2$blendColour intensity clr clr2

{-# INLINE addColour #-}
addColour :: POINT -> COLORREF -> Draw()
addColour pt clr = do
	transform <- askTransform
	let pt2 = linearMap transform pt
	b <- inBounds pt2
	when b$do
		clr2 <- unsafeGetP pt2
		let formula f = fromIntegral(255`min`(fromIntegral(f clr2)+fromIntegral(f clr))::Int32)
		unsafeSetP pt2$rgb
			(formula getRValue)
			(formula getGValue)
			(formula getBValue)

{-# INLINE tint #-}
tint :: COLORREF -> COLORREF -> COLORREF
tint c c2 = unTuple$enTuple c * enTuple c2 / 255
{-# INLINE brightness #-}
brightness :: Double -> COLORREF -> COLORREF
brightness n c = unTuple$fromRational(toRational n)*enTuple c

evalTriple = evalTuple3 rseq rseq rseq
instance (Num t, Num u, Num v) => Num (t, u, v) where
	(n1, n2, n3) + (m1, m2, m3) = (n1 + m1, n2 + m2, n3 + m3)`using`evalTriple
	negate (n1, n2, n3) = (-n1, -n2, -n3)`using`evalTriple
	(n1, n2, n3) * (m1, m2, m3) = (n1 * m1, n2 * m2, n3 * m3)`using`evalTriple
	signum (n1, n2, n3) = (signum n1, signum n2, signum n3)`using`evalTriple
	abs (n1, n2, n3) = (abs n1, abs n2, abs n3)`using`evalTriple
	fromInteger n = (fromInteger n, fromInteger n, fromInteger n)`using`evalTriple

instance (Fractional t, Fractional u, Fractional v) => Fractional (t, u, v) where
	fromRational n = (fromRational n, fromRational n, fromRational n)`using`evalTriple
	recip (n1, n2, n3) = (recip n1, recip n2, recip n3)`using`evalTriple

enTuple :: COLORREF -> (Float,Float,Float)
enTuple x = (fromIntegral$getRValue x, fromIntegral$getGValue x, fromIntegral$getBValue x)

unTuple :: (Float,Float,Float) -> COLORREF
unTuple (r, g, b) = rgb (round r) (round g) (round b)

unsafeConcF_ :: Int->(Int->Draw())->Draw()
unsafeConcF_ n f = do
	let ?seq = True
	let ?pool = BoxedThreadPool NoPool
	tup <- unsafeAsk
	unsafeLiftIO$concF_ n$unwrapDraw tup.f
{-# LANGUAGE Unsafe, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, ImplicitParams, ForeignFunctionInterface, FlexibleContexts, TypeFamilies, TupleSections #-}

module DrawLib.Text where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
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
import Graphics.Win32 hiding (textOut)
import qualified Graphics.Win32
import Graphics.Win32Extras
import DrawLib
import Control.FinallyHandler
import Prelude hiding (replicate)

type Font = (COLORREF, COLORREF, String, Int32, DWORD, Bool, Bool)

-- | A default font option
defFont :: Font
defFont = (defBackground, 0, "Tahoma", 14, fW_NORMAL, False, False)

setupTextDrawing :: FinallyHandler -> Font -> HDC -> IO()
setupTextDrawing r (_, _, font, sz, weight, italic, underline) cdc = do
	-- Create font.
	fhdl <- createFont(sz * 72 `quot` 96) 0 0 0 weight italic underline False dEFAULT_CHARSET 0 0 0 0 font
	addFinallyHandler r(deleteFont fhdl)
	oldFont <- selectFont cdc fhdl
	addFinallyHandler r(selectFont cdc oldFont)

{--- | Get the dimensions of a piece of text in a given font. (Note: this may be slightly different
--   on different platforms).
textDimensions :: String -> Font -> SIZE
textDimensions text font = unsafePerformIO$newFinallyHandler>>= \r->
	finally' r$do
	(_, cdc) <- constructTextDrawingData r font
	getTextExtentPoint32 cdc text-}

-- | Draw some text at the specified point in the bitmap.
textOut :: String -> POINT -> Font -> Draw()
textOut text pt font@(fgclr, bkclr, _, _, _, _, _) = do
	((wd, ht), transform, _, cdc) <- unsafeAsk
	let (x2, y2) = linearMap transform pt
	unsafeLiftIO$newFinallyHandler>>= \r->finally' r$do
		setupTextDrawing r font cdc
		setTextColor cdc fgclr
		setBkColor cdc bkclr
		setBkMode cdc oPAQUE
		drawText cdc text(x2, y2, wd, ht) 0
		void gdiFlush
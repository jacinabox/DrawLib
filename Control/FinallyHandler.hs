module Control.FinallyHandler (FinallyHandler, newFinallyHandlerResetter, newFinallyHandler, addFinallyHandler, finally') where

import Control.Monad
import Data.IORef
import Control.Exception

-- | This module implements patterns involving 'finally'. Keeping track of the operations to be done
-- in a 'finally' block helps to keep the code clear of clutter.
newtype FinallyHandler = FinallyHandler(IORef(IO()))

-- | Constructs a 'FinallyHandler' and a gadget to prevent any operations from being performed
-- (a resetter).
{-# INLINE newFinallyHandlerResetter #-}
newFinallyHandlerResetter :: IO(FinallyHandler, IO())
newFinallyHandlerResetter = liftM(\r -> (FinallyHandler r, writeIORef r(return ()))) (newIORef(return()))
-- | A variant of 'newFinallyHandlerResetter' without the resetter.
{-# INLINE newFinallyHandler #-}
newFinallyHandler :: IO FinallyHandler
newFinallyHandler = liftM FinallyHandler(newIORef(return()))
{-# INLINE addFinallyHandler #-}
-- | Adds an operation to the 'FinallyHandler'. Operations are done in the opposite order they are
-- registered in; this is appropriate for resource management.
addFinallyHandler :: FinallyHandler -> IO t -> IO()
addFinallyHandler (FinallyHandler r) m = modifyIORef' r(m>>)
-- | The pattern of use for a 'FinallyHandler'.
{-# INLINE finally' #-}
finally' :: FinallyHandler -> IO t -> IO t
finally'(FinallyHandler r) m = finally m(join(readIORef r)>>writeIORef r(return()))

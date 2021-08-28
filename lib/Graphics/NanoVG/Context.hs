{-|
Module      : Graphics.NanoVG.Context
Description : NanoVG context: initialization, manipulation and types.
Copyright   : (c) Keny C, 2021
License     : MIT
Stability   : experimental

This module defines functions to initialize a NanoVG context.
This module also defines the 'VG' monad within which NanoVG drawing instructions are called. 
When given a NanoVG context to execute into, 'VG' instructions can then be turned into 'IO' computations (cf 'frame' and 'withContext'). 
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.NanoVG.Context where

import Foreign.Ptr
import Foreign.ForeignPtr
--
import Linear.V2
--
import Control.Monad.Reader
import Control.Monad.IO.Class ()

import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.State
import Graphics.NanoVG.Internal.Flag

-- | NanoVG context. All drawing instructions are evaluated in one such context. Use 'VG' monad to abstract away the context.
data NVGContext = NVGContext {
    _getNVGContext :: !(ForeignPtr ())
}

-- | Window resolution. Needed by 'frame'.
data WindowResolution = WindowResolution {
    _size     :: !(V2 Float), -- ^ (V2 width height) in pixels
    _pxRatio  :: !Float       -- ^ Number of physical pixels per number of logical pixels. Typically 1. On certain monitors, the value is higher than 1. To know the exact pixel ratio, divide OpenGL frame buffer size by window size. For more info, cf HiDPI or Apple Retina Display.
}

-- | Monad for drawing instructions. 
--   Under the hood, a reader monad over IO, with NanoVG context as parameter
newtype VG a = VG {
    unwrapVG :: ReaderT NVGContext IO a
} deriving (Functor, Applicative, Monad, MonadIO)


-- | Performs vector graphics drawing in the given context
withContext :: NVGContext -- ^ NanoVG context
            -> VG a       -- ^ drawing instructions
            -> IO a
withContext context (VG body) = runReaderT body context


applyContext :: (Ptr () -> IO a) -> VG a
applyContext function = VG $ withReaderT _getNVGContext $ do
                            foreignPtr <- ask
                            liftIO $ withForeignPtr foreignPtr function


data InitFlag = Antialias      -- ^ Whether drawings should-be anti-aliased
              | StencilStrokes -- ^ Whether strokes be drawn using the stencil buffer. Slower rendering but overlapping path are drawn just once.
              | Debug          -- ^ Whether additional debug checks on OpenGL calls should be run. 
              deriving (Eq, Show)

instance Flag InitFlag where
    toCInt Antialias      = _antialias
    toCInt StencilStrokes = _stencil_strokes
    toCInt Debug          = _debug


-- | Creates a NanoVG context for OpenGL 3.
nvgGL3Context :: [InitFlag] -> IO NVGContext
nvgGL3Context flags = do 
    pointer    <- c_createGL3 $ compileFlags flags
    foreignPtr <- newForeignPtr c_destroyGL3 pointer
    return $ NVGContext foreignPtr


-- | Pushes current drawing state (i.e. stroke color, transform, fill paint, etc.) to a stack of states. 
--   Then sets current drawing state back to default values.
--   Used in combination with 'restore'.
save :: VG ()
save = applyContext c_save

-- | Pops drawing state from stack (i.e. stroke color, transform, fill paint, etc.) and sets it to current state
--   Used in combination with 'save'.
restore :: VG ()
restore = applyContext c_restore

-- | Executes all commands in the given scope in new default drawing state. Previous drawing state is restored upon exit of the scope argument.
withNewState :: VG a  -- ^ Scope to be executed in new default drawing state
             -> VG a
withNewState cont = do
    save
    toReturn <- cont
    restore
    return toReturn

-- | Resets drawing state to default initial values.
reset :: VG ()
reset = applyContext c_reset
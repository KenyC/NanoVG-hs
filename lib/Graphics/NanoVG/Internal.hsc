{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.NanoVG.Internal where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr


#include "nanovg.h"
#include "nanovg_gl.h"
#include "nanovg_hs_wrapper.h"

foreign import ccall "nanovg_gl.h nvgCreateGL3"
    c_createGL3 :: CInt -> IO (Ptr ())

foreign import ccall "nanovg_gl.h &nvgDeleteGL3"
    c_destroyGL3 :: FinalizerPtr ()

-- foreign import ccall "nanovg_gl.h nvgDeleteGL3"
--     c_destroyGL :: Ptr () -> IO ()



foreign import ccall unsafe "nanovg.h nvgBeginFrame"
    c_beginFrame :: Ptr ()
                 -> CFloat
                 -> CFloat
                 -> CFloat
                 -> IO ()

foreign import ccall unsafe "nanovg.h nvgEndFrame"
    c_endFrame :: Ptr ()
               -> IO ()





------------------- C to Haskell translation utils -----------------


#{enum CInt, 
  , _antialias             = NVG_ANTIALIAS
  , _stencil_strokes       = NVG_STENCIL_STROKES
  , _debug                 = NVG_DEBUG
}



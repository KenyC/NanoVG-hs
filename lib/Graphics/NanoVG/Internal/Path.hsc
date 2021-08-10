module Graphics.NanoVG.Internal.Path where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr

-- #include "nanovg.h"
-- #include "nanovg_gl.h"
-- #include "nanovg_hs_wrapper.h"

foreign import ccall unsafe "nanovg.h nvgBeginPath"
    c_beginPath :: Ptr () -> IO ()


foreign import ccall unsafe "nanovg.h nvgMoveTo"
    c_moveTo :: Ptr () 
             -> CFloat
             -> CFloat
             -> IO ()

foreign import ccall unsafe "nanovg.h nvgLineTo"
    c_lineTo :: Ptr () 
             -> CFloat
             -> CFloat
             -> IO ()

foreign import ccall unsafe "nanovg.h nvgBezierTo"
    c_bezierTo :: Ptr () 
               -> CFloat
               -> CFloat
               -> CFloat
               -> CFloat
               -> CFloat
               -> CFloat
               -> IO ()

foreign import ccall unsafe "nanovg.h nvgArcTo"
    c_arcTo :: Ptr () 
            -> CFloat
            -> CFloat
            -> CFloat
            -> CFloat
            -> CFloat
            -> IO ()

foreign import ccall unsafe "nanovg.h nvgQuadTo"
    c_quadTo :: Ptr () 
               -> CFloat
               -> CFloat
               -> CFloat
               -> CFloat
               -> IO ()

foreign import ccall unsafe "nanovg.h nvgClosePath"
    c_closePath :: Ptr () -> IO ()
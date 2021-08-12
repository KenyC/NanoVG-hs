module Graphics.NanoVG.Internal.Path where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr

#include "nanovg.h"
-- #include "nanovg_gl.h"
-- #include "nanovg_hs_wrapper.h"

#{enum CInt,
  , _clockwise        = NVG_CW
  , _counterClockwise = NVG_CCW
}

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


foreign import ccall unsafe "nanovg.h nvgArc"
    -- | Creates new circle arc shaped sub-path. The arc center is at cx,cy, the arc radius is r,
    --   and the arc is drawn from angle a0 to a1, and swept in direction dir (NVG_CCW, or NVG_CW).
    --   Angles are specified in radians.
    c_arc :: Ptr ()
          -> CFloat -> CFloat 
          -> CFloat
          -> CFloat -> CFloat 
          -> CInt
          -> IO ()

foreign import ccall unsafe "nanovg.h nvgRect"
    -- | Creates new rectangle shaped sub-path.
    c_rect :: Ptr ()
           -> CFloat
           -> CFloat
           -> CFloat
           -> CFloat
           -> IO ()

foreign import ccall unsafe "nanovg.h nvgRoundedRect"
    -- | Creates new rounded rectangle shaped sub-path.
    c_roundedRect :: Ptr ()
                  -> CFloat -> CFloat 
                  -> CFloat -> CFloat 
                  -> CFloat
                  -> IO ()

foreign import ccall unsafe "nanovg.h nvgRoundedRectVarying"
    -- | Creates new rounded rectangle shaped sub-path with varying radii for each corner.
    c_roundedRectVarying :: Ptr ()
                         -> CFloat -> CFloat 
                         -> CFloat -> CFloat 
                         -> CFloat -> CFloat -> CFloat -> CFloat
                         -> IO ()

foreign import ccall unsafe "nanovg.h nvgEllipse"
    -- | Creates new ellipse shaped sub-path.
    c_ellipse :: Ptr ()
              -> CFloat -> CFloat 
              -> CFloat -> CFloat 
              -> IO ()

foreign import ccall unsafe "nanovg.h nvgCircle"
    -- | Creates new circle shaped sub-path.
    c_circle :: Ptr ()
             -> CFloat -> CFloat 
             -> CFloat
             -> IO ()

module Graphics.NanoVG.Internal.Path where

import Foreign.C.Types
import Foreign.Ptr

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



#{enum CInt,
    , _solid = NVG_SOLID 
    , _hole  = NVG_HOLE  
}

-- | Sets the current sub-path winding, see NVGwinding and NVGsolidity.
foreign import ccall unsafe "nanovg.h nvgPathWinding"
    c_pathWinding :: Ptr () -> CInt -> IO ()


-- | Creates new circle arc shaped sub-path. The arc center is at cx,cy, the arc radius is r,
--   and the arc is drawn from angle a0 to a1, and swept in direction dir (NVG_CCW, or NVG_CW).
--   Angles are specified in radians.
foreign import ccall unsafe "nanovg.h nvgArc"
    c_arc :: Ptr ()
          -> CFloat -> CFloat 
          -> CFloat
          -> CFloat -> CFloat 
          -> CInt
          -> IO ()

-- | Creates new rectangle shaped sub-path.
foreign import ccall unsafe "nanovg.h nvgRect"
    c_rect :: Ptr ()
           -> CFloat
           -> CFloat
           -> CFloat
           -> CFloat
           -> IO ()

-- | Creates new rounded rectangle shaped sub-path.
foreign import ccall unsafe "nanovg.h nvgRoundedRect"
    c_roundedRect :: Ptr ()
                  -> CFloat -> CFloat 
                  -> CFloat -> CFloat 
                  -> CFloat
                  -> IO ()

-- | Creates new rounded rectangle shaped sub-path with varying radii for each corner.
foreign import ccall unsafe "nanovg.h nvgRoundedRectVarying"
    c_roundedRectVarying :: Ptr ()
                         -> CFloat -> CFloat 
                         -> CFloat -> CFloat 
                         -> CFloat -> CFloat -> CFloat -> CFloat
                         -> IO ()

-- | Creates new ellipse shaped sub-path.
foreign import ccall unsafe "nanovg.h nvgEllipse"
    c_ellipse :: Ptr ()
              -> CFloat -> CFloat 
              -> CFloat -> CFloat 
              -> IO ()

-- | Creates new circle shaped sub-path.
foreign import ccall unsafe "nanovg.h nvgCircle"
    c_circle :: Ptr ()
             -> CFloat -> CFloat 
             -> CFloat
             -> IO ()

module Graphics.NanoVG.Internal.Draw where

import Foreign.C.Types
import Foreign.Ptr

#include "nanovg.h"
-- #include "nanovg_gl.h"
-- #include "nanovg_hs_wrapper.h"

#{enum CInt,
  , _source_over      = NVG_SOURCE_OVER
  , _source_in        = NVG_SOURCE_IN
  , _source_out       = NVG_SOURCE_OUT
  , _source_atop      = NVG_ATOP
  , _destination_over = NVG_DESTINATION_OVER
  , _destination_in   = NVG_DESTINATION_IN
  , _destination_out  = NVG_DESTINATION_OUT
  , _destination_atop = NVG_DESTINATION_ATOP
  , _lighter          = NVG_LIGHTER
  , _copy             = NVG_COPY
  , _xor              = NVG_XOR
}

------------------- DRAW FUNCTIONS -----------------




foreign import ccall unsafe "nanovg.h nvgStroke"
    c_stroke :: Ptr ()
             -> IO ()

foreign import ccall unsafe "nanovg.h nvgStrokeWidth"
    c_strokeWidth :: Ptr ()
                  -> CFloat
                  -> IO ()

foreign import ccall unsafe "nanovg.h nvgFill"
    c_fill :: Ptr ()
           -> IO ()


foreign import ccall unsafe "nanovg.h nvgFillColorHs"
    c_fillColor :: Ptr ()
                -> CFloat -- ^ red
                -> CFloat -- ^ green
                -> CFloat -- ^ blue
                -> CFloat -- ^ alpha
                -> IO ()

foreign import ccall unsafe "nanovg.h nvgStrokeColorHs"
    c_strokeColor :: Ptr ()
                  -> Float -- ^ red
                  -> Float -- ^ green
                  -> Float -- ^ blue
                  -> Float -- ^ alpha
                  -> IO ()

-- | Sets the composite operation. The op parameter should be one of NVGcompositeOperation.
foreign import ccall unsafe "nanovg.h nvgGlobalCompositeOperation"
    c_globalCompositeOperation :: Ptr () 
                               -> CInt    -- ^ op
                               -> IO ()

-- | Sets the composite operation with custom pixel arithmetic. The parameters should be one of NVGblendFactor.
foreign import ccall unsafe "nanovg.h nvgGlobalCompositeBlendFunc"
    c_globalCompositeBlendFunc :: Ptr ()
                               -> CInt    --  ^ src
                               -> CInt    --  ^ dest
                               -> IO ()

-- | Sets the composite operation with custom pixel arithmetic for RGB and alpha components separately. The parameters should be one of NVGblendFactor.
foreign import ccall unsafe "nanovg.h nvgGlobalCompositeBlendFuncSeparate"
    c_globalCompositeBlendFuncSeparate 
        :: Ptr ()
        -> CInt     -- ^ srcRGB
        -> CInt     -- ^ dstRGB
        -> CInt     -- ^ srcAlpha
        -> CInt     -- ^ dstAlpha
        -> IO ()


-- | Sets whether to draw antialias for nvgStroke() and nvgFill(). It's enabled by default.
foreign import ccall unsafe "nanovg.h nvgShapeAntiAlias"
    c_shapeAntiAlias :: Ptr () 
                     -> CInt
                     -> IO ()


-- | Sets the miter limit of the stroke style.
--   Miter limit controls when a sharp corner is beveled.
foreign import ccall unsafe "nanovg.h nvgMiterLimit"
    c_miterLimit :: Ptr () 
                 -> CFloat
                 -> IO ()





#{enum CInt,
  , _butt      = NVG_BUTT
  , _round     = NVG_ROUND
  , _square    = NVG_SQUARE
  , _bevel     = NVG_BEVEL
  , _miter     = NVG_MITER
}


-- | Sets how the end of the line (cap) is drawn,
--   Can be one of: NVG_BUTT (default), NVG_ROUND, NVG_SQUARE.
foreign import ccall unsafe "nanovg.h nvgLineCap"
    c_lineCap :: Ptr () 
              -> CInt
              -> IO ()

-- | Sets how sharp path corners are drawn.
--   Can be one of NVG_MITER (default), NVG_ROUND, NVG_BEVEL.
foreign import ccall unsafe "nanovg.h nvgLineJoin"
    c_lineJoin ::  Ptr () 
                -> CInt
                -> IO ()

-- | Sets the transparency applied to all rendered shapes.
--   Already transparent paths will get proportionally more transparent as well.
foreign import ccall unsafe "nanovg.h nvgGlobalAlpha"
    c_globalAlpha :: Ptr () 
                  -> CFloat
                  -> IO ()

module Graphics.NanoVG.Internal.Draw where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr

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


foreign import ccall unsafe "nanovg.h nvgGlobalCompositeOperation"
    -- Sets the composite operation. The op parameter should be one of NVGcompositeOperation.
    c_globalCompositeOperation :: Ptr () 
                               -> CInt    -- ^ op
                               -> IO ()

foreign import ccall unsafe "nanovg.h nvgGlobalCompositeBlendFunc"
    -- Sets the composite operation with custom pixel arithmetic. The parameters should be one of NVGblendFactor.
    c_globalCompositeBlendFunc :: Ptr ()
                               -> CInt    --  ^ src
                               -> CInt    --  ^ dest
                               -> IO ()

foreign import ccall unsafe "nanovg.h nvgGlobalCompositeBlendFuncSeparate"
    -- Sets the composite operation with custom pixel arithmetic for RGB and alpha components separately. The parameters should be one of NVGblendFactor.
    c_globalCompositeBlendFuncSeparate 
        :: Ptr ()
        -> CInt     -- ^ srcRGB
        -> CInt     -- ^ dstRGB
        -> CInt     -- ^ srcAlpha
        -> CInt     -- ^ dstAlpha
        -> IO ()


foreign import ccall unsafe "nanovg.h nvgShapeAntiAlias"
    -- | Sets whether to draw antialias for nvgStroke() and nvgFill(). It's enabled by default.
    c_shapeAntiAlias :: Ptr () 
                     -> CInt
                     -> IO ()


foreign import ccall unsafe "nanovg.h nvgMiterLimit"
    -- | Sets the miter limit of the stroke style.
    --   Miter limit controls when a sharp corner is beveled.
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


foreign import ccall unsafe "nanovg.h nvgLineCap"
    -- | Sets how the end of the line (cap) is drawn,
    --   Can be one of: NVG_BUTT (default), NVG_ROUND, NVG_SQUARE.
    c_lineCap :: Ptr () 
              -> CInt
              -> IO ()

foreign import ccall unsafe "nanovg.h nvgLineJoin"
    -- | Sets how sharp path corners are drawn.
    --   Can be one of NVG_MITER (default), NVG_ROUND, NVG_BEVEL.
    c_lineJoin ::  Ptr () 
                -> CInt
                -> IO ()

foreign import ccall unsafe "nanovg.h nvgGlobalAlpha"
    -- | Sets the transparency applied to all rendered shapes.
    --   Already transparent paths will get proportionally more transparent as well.
    c_globalAlpha :: Ptr () 
                  -> CFloat
                  -> IO ()

module Graphics.NanoVG.Internal.Paint where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr


-- | Sets current stroke style to a paint, which can be a one of the gradients or a pattern.
foreign import ccall unsafe "nanovg_hs_wrapper.h nvgStrokePaintHs"
    c_strokePaint ::  Ptr ()
                   -> Ptr ()
                   -> IO ()

-- | Sets current fill style to a paint, which can be a one of the gradients or a pattern.
foreign import ccall unsafe "nanovg_hs_wrapper.h nvgFillPaintHs"
    c_fillPaint ::  Ptr ()
                 -> Ptr ()
                 -> IO ()



-- | Creates and returns a linear gradient. Parameters (sx,sy)-(ex,ey) specify the start and end coordinates
--   of the linear gradient, icol specifies the start color and ocol the end color.
--   The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
foreign import ccall unsafe "nanovg_hs_wrapper.h nvgLinearGradientHs"
    c_linearGradient :: Ptr ()
                     -> CFloat -> CFloat
                     -> CFloat -> CFloat
                     -> CFloat -> CFloat -> CFloat -> CFloat
                     -> CFloat -> CFloat -> CFloat -> CFloat
                     -> IO (Ptr ())

-- | Creates and returns a box gradient. Box gradient is a feathered rounded rectangle, it is useful for rendering
--   drop shadows or highlights for boxes. Parameters (x,y) define the top-left corner of the rectangle,
--   (w,h) define the size of the rectangle, r defines the corner radius, and f feather. Feather defines how blurry
--   the border of the rectangle is. Parameter icol specifies the inner color and ocol the outer color of the gradient.
--   The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
foreign import ccall unsafe "nanovg_hs_wrapper.h nvgBoxGradientHs"
    c_boxGradient :: Ptr ()
                  -> CFloat -> CFloat
                  -> CFloat -> CFloat
                  -> CFloat -> CFloat
                  -> CFloat -> CFloat -> CFloat -> CFloat
                  -> CFloat -> CFloat -> CFloat -> CFloat
                  -> IO (Ptr ())

-- | Creates and returns a radial gradient. Parameters (cx,cy) specify the center, inr and outr specify
--   the inner and outer radius of the gradient, icol specifies the start color and ocol the end color.
--   The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
foreign import ccall unsafe "nanovg_hs_wrapper.h nvgRadialGradientHs"
    c_radialGradient :: Ptr ()
                     -> CFloat -> CFloat
                     -> CFloat -> CFloat
                     -> CFloat -> CFloat -> CFloat -> CFloat
                     -> CFloat -> CFloat -> CFloat -> CFloat
                     -> IO (Ptr ())

-- | Creates and returns an image pattern. Parameters (ox,oy) specify the left-top location of the image pattern,
--   (ex,ey) the size of one image, angle rotation around the top-left corner, image is handle to the image to render.
--   The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
foreign import ccall unsafe "nanovg_hs_wrapper.h nvgImagePatternHs"
    c_imagePattern :: Ptr ()
                   -> CFloat -> CFloat
                   -> CFloat -> CFloat
                   -> CFloat -> CInt   -> CFloat 
                   -> IO (Ptr ())

-- foreign import ccall unsafe "nanovg_hs_wrapper.h printNvgPaint"
--     c_printPaint :: Ptr ()
--                  -> IO ()


module Graphics.NanoVG.Internal.Transform where

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall unsafe "nanovg.h nvgResetTransform"
    -- | Resets current transform to a identity matrix.
    c_resetTransform :: Ptr () -> IO ()

foreign import ccall unsafe "nanovg.h nvgTransform"
    -- | Premultiplies current coordinate system by specified matrix.
    --   The parameters are interpreted as matrix as follows:
    --     [a c e]
    --     [b d f]
    --     [0 0 1]
    c_transform :: Ptr ()
                -> CFloat 
                -> CFloat 
                -> CFloat 
                -> CFloat 
                -> CFloat 
                -> CFloat 
                -> IO ()

foreign import ccall unsafe "nanovg.h nvgTranslate"
    c_translate :: Ptr ()
                -> CFloat 
                -> CFloat
                -> IO ()

foreign import ccall unsafe "nanovg.h nvgRotate"
    -- | Rotates current coordinate system. Angle is specified in radians.
     c_rotate :: Ptr () -> CFloat -> IO ()

foreign import ccall unsafe "nanovg.h nvgSkewX"
    -- | Skews the current coordinate system along X axis. Angle is specified in radians.
    c_skewX :: Ptr ()
            -> CFloat
            -> IO ()

foreign import ccall unsafe "nanovg.h nvgSkewY"
    -- | Skews the current coordinate system along Y axis. Angle is specified in radians.
    c_skewY :: Ptr ()
            -> CFloat
            -> IO ()


foreign import ccall unsafe "nanovg.h nvgScale"
    -- | Scales the current coordinate system.
    c_scale :: Ptr () -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "nanovg.h nvgCurrentTransform"
    -- | Stores the top part (a-f) of the current transformation matrix in to the specified buffer.
    --   [a c e]
    --   [b d f]
    --   [0 0 1]
    --  There should be space for 6 floats in the return buffer for the values a-f.
    c_currentTransform :: Ptr () -> Ptr CFloat -> IO ()



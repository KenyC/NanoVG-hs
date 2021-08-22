module Graphics.NanoVG.Internal.Transform where

import Foreign.C.Types
import Foreign.Ptr

-- | Resets current transform to a identity matrix.
foreign import ccall unsafe "nanovg.h nvgResetTransform"
    c_resetTransform :: Ptr () -> IO ()

-- | Premultiplies current coordinate system by specified matrix.
--   The parameters are interpreted as matrix as follows:
--     [a c e]
--     [b d f]
--     [0 0 1]
foreign import ccall unsafe "nanovg.h nvgTransform"
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

-- | Rotates current coordinate system. Angle is specified in radians.
foreign import ccall unsafe "nanovg.h nvgRotate"
     c_rotate :: Ptr () -> CFloat -> IO ()

-- | Skews the current coordinate system along X axis. Angle is specified in radians.
foreign import ccall unsafe "nanovg.h nvgSkewX"
    c_skewX :: Ptr ()
            -> CFloat
            -> IO ()

-- | Skews the current coordinate system along Y axis. Angle is specified in radians.
foreign import ccall unsafe "nanovg.h nvgSkewY"
    c_skewY :: Ptr ()
            -> CFloat
            -> IO ()


-- | Scales the current coordinate system.
foreign import ccall unsafe "nanovg.h nvgScale"
    c_scale :: Ptr () -> CFloat -> CFloat -> IO ()

-- | Stores the top part (a-f) of the current transformation matrix in to the specified buffer.
--   [a c e]
--   [b d f]
--   [0 0 1]
--  There should be space for 6 floats in the return buffer for the values a-f.
foreign import ccall unsafe "nanovg.h nvgCurrentTransform"
    c_currentTransform :: Ptr () -> Ptr CFloat -> IO ()



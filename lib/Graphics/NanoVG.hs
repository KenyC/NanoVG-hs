module Graphics.NanoVG(
    -- Re-exports
    nvgGL3Context,
    NVGContext(..),
    WindowResolution(..),
    debug,
    antialias,

    --
    withFrame
) where

import Foreign.Ptr
import Foreign.ForeignPtr
--
import Linear.V2

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal

withFrame :: WindowResolution 
          -> VG () 
          -> VG ()
withFrame (WindowResolution (V2 width height) dpi) cont = do 
    applyContext $ \ptr ->
        c_beginFrame 
            ptr 
            (realToFrac width) 
            (realToFrac height) 
            (realToFrac dpi)
    cont
    applyContext c_endFrame
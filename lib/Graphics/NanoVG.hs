module Graphics.NanoVG(
    -- Re-exports
    nvgGL3Context,
    NVGContext(..),
    WindowResolution(..),
    debug,
    antialias,

    --
    frame
) where

import Foreign.Ptr
import Foreign.ForeignPtr
--
import Linear.V2

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal

frame :: NVGContext
      -> WindowResolution 
      -> VG () 
      -> IO ()
frame 
    context@(NVGContext foreignPtr) 
    (WindowResolution (V2 width height) dpi) 
    cont = do 
                withForeignPtr foreignPtr $ \ptr -> do
                    c_beginFrame 
                        ptr 
                        (realToFrac width) 
                        (realToFrac height) 
                        (realToFrac dpi)
                    withContext context cont
                    c_endFrame ptr
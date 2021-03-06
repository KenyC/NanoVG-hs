module Graphics.NanoVG.Internal.Scissor where

import Foreign.C.Types
import Foreign.Ptr


-- | Sets the current scissor rectangle.
--   The scissor rectangle is transformed by the current transform.
foreign import ccall unsafe "nanovg.h nvgScissor"
    c_scissor :: Ptr ()
              -> CFloat
              -> CFloat
              -> CFloat
              -> CFloat
              -> IO ()

-- | Intersects current scissor rectangle with the specified rectangle.
--   The scissor rectangle is transformed by the current transform.
--   Note: in case the rotation of previous scissor rect differs from
--   the current one, the intersection will be done between the specified
--   rectangle and the previous scissor rectangle transformed in the current
--   transform space. The resulting shape is always rectangle.
foreign import ccall unsafe "nanovg.h nvgIntersectScissor"
    c_intersectScissor :: Ptr ()
                       -> CFloat
                       -> CFloat
                       -> CFloat
                       -> CFloat
                       -> IO ()


-- | Reset and disables scissoring.
foreign import ccall unsafe "nanovg.h nvgResetScissor"
    c_resetScissor :: Ptr () -> IO ()


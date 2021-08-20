module Graphics.NanoVG.Internal.Scissor where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr


foreign import ccall unsafe "nanovg.h nvgScissor"
    -- | Sets the current scissor rectangle.
    --   The scissor rectangle is transformed by the current transform.
    c_scissor :: Ptr ()
              -> CFloat
              -> CFloat
              -> CFloat
              -> CFloat
              -> IO ()

foreign import ccall unsafe "nanovg.h nvgIntersectScissor"
    -- | Intersects current scissor rectangle with the specified rectangle.
    --   The scissor rectangle is transformed by the current transform.
    --   Note: in case the rotation of previous scissor rect differs from
    --   the current one, the intersection will be done between the specified
    --   rectangle and the previous scissor rectangle transformed in the current
    --   transform space. The resulting shape is always rectangle.
    c_intersectScissor :: Ptr ()
                       -> CFloat
                       -> CFloat
                       -> CFloat
                       -> CFloat
                       -> IO ()


foreign import ccall unsafe "nanovg.h nvgResetScissor"
    -- | Reset and disables scissoring.
    c_resetScissor :: Ptr () -> IO ()


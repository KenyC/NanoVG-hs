module Graphics.NanoVG.Internal.State where

import Foreign.Ptr

-- |  Pushes and saves the current render state into a state stack.
--    A matching nvgRestore() must be used to restore the state.
foreign import ccall unsafe "nanovg.h nvgSave"
    c_save :: Ptr () -> IO ()

-- |  Pops and restores current render state.
foreign import ccall unsafe "nanovg.h nvgRestore"
    c_restore :: Ptr () -> IO ()

-- |  Resets current render state to default values. Does not affect the render state stack.
foreign import ccall unsafe "nanovg.h nvgReset"
    c_reset :: Ptr () -> IO ()


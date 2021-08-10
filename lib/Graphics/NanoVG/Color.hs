{-# LANGUAGE RecordWildCards #-}
module Graphics.NanoVG.Color where

import Foreign.ForeignPtr

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal

data Color = Color {
    _red   :: Float,
    _green :: Float,
    _blue  :: Float,
    _alpha :: Float
} deriving (Eq, Show)



fillColor :: Color
          -> VG ()
fillColor Color{..} = 
    applyContext $ \ptr ->
        c_fillColor ptr 
        (realToFrac _red) 
        (realToFrac _green) 
        (realToFrac _blue) 
        (realToFrac _alpha)

strokeColor :: Color
            -> VG ()
strokeColor Color{..} = 
    applyContext $ \ptr ->
        c_strokeColor ptr 
        (realToFrac _red) 
        (realToFrac _green) 
        (realToFrac _blue) 
        (realToFrac _alpha)

{-# LANGUAGE RecordWildCards #-}
module Graphics.NanoVG.Color where

import Foreign.ForeignPtr

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal

-- | RGBA representation of color
data Color = Color {
    _red   :: Float, -- ^ red value, from 0 to 1 
    _green :: Float, -- ^ green value, from 0 to 1 
    _blue  :: Float, -- ^ blue value, from 0 to 1 
    _alpha :: Float  -- ^ alpha value, from 0 to 1
} deriving (Eq, Show)


-- | Sets color of filling  
fillColor :: Color
          -> VG ()
fillColor Color{..} = 
    applyContext $ \ptr ->
        c_fillColor ptr 
        (realToFrac _red) 
        (realToFrac _green) 
        (realToFrac _blue) 
        (realToFrac _alpha)

-- | Sets color of strokes
strokeColor :: Color
            -> VG ()
strokeColor Color{..} = 
    applyContext $ \ptr ->
        c_strokeColor ptr 
        (realToFrac _red) 
        (realToFrac _green) 
        (realToFrac _blue) 
        (realToFrac _alpha)

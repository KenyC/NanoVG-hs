{-# LANGUAGE RecordWildCards #-}
module Graphics.NanoVG.Draw where

import Foreign.ForeignPtr
--
import Linear.V2

import Graphics.NanoVG.Context
import Graphics.NanoVG.Color
import Graphics.NanoVG.Internal


-- | Draws previous path as a series of strokes
stroke :: VG ()
stroke = applyContext c_stroke

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


strokeWidth :: Float
            -> VG ()
strokeWidth width = applyContext $ \ptr -> c_strokeWidth ptr (realToFrac width)

-- | Fills area within previous path with color
fill :: VG ()
fill = applyContext c_fill

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



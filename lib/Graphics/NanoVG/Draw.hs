module Graphics.NanoVG.Draw where

import Foreign.ForeignPtr
--
import Linear.V2

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal



stroke :: VG ()
stroke = applyContext c_stroke

fill :: VG ()
fill = applyContext c_fill

rect :: V2 Float
     -> V2 Float
     -> VG ()
rect
    (V2 x y)
    (V2 w h) =
    applyContext $ \ptr -> 
        c_rect ptr 
            (realToFrac x) (realToFrac y) 
            (realToFrac w) (realToFrac h)
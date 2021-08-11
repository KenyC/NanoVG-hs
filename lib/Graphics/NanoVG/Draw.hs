module Graphics.NanoVG.Draw where

import Foreign.ForeignPtr
--
import Linear.V2

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal


-- | Draws previous path as a series of strokes
stroke :: VG ()
stroke = applyContext c_stroke

-- | Fills area within previous path with color
fill :: VG ()
fill = applyContext c_fill



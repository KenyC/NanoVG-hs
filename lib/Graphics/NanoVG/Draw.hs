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



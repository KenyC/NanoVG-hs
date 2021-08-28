module WindowState where

import Linear.V2
import FpsWidget
import Graphics.NanoVG.Text
import Graphics.NanoVG.Image

data WindowState = WindowState {
    time          :: !Float,
    mousePosition :: !(V2 Float),
    graph         :: !FpsGraph,
    boldFont      :: !Font,
    normalFont    :: !Font,
    iconFont      :: !Font,
    images        :: ![Image]
}
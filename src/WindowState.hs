module WindowState where

import Linear.V2
import FpsWidget
import Graphics.NanoVG.Text

data WindowState = WindowState {
    time :: !Float,
    mousePosition :: !(V2 Float),
    graph :: !FpsGraph,
    font  :: !Font
}
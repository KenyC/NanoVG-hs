module WindowState where

import Linear.V2
import FpsWidget

data WindowState = WindowState {
    time :: !Float,
    mousePosition :: !(V2 Float),
    graph :: !FpsGraph
}
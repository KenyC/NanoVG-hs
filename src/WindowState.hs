module WindowState where

import Linear.V2

data WindowState = WindowState {
    time :: Float,
    mousePosition :: V2 Float
}
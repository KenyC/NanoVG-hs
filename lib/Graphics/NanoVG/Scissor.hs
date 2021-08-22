{-|
Module      : Graphics.NanoVG.Scissor
Description : Clipping drawings.
Copyright   : (c) Keny C, 2021
License     : MIT
Stability   : experimental

This module defines function that let you clip any drawing outside a certain rectangular area (called "scissor"). The area is always axis-aligned.
-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.NanoVG.Scissor where

import Linear.V2

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal.Scissor


scissor :: V2 Float
        -> V2 Float
        -> VG ()
scissor (V2 x y) (V2 w h) = 
    applyContext $ \ptr -> c_scissor 
                                ptr 
                                (realToFrac x) (realToFrac y)
                                (realToFrac w) (realToFrac h)

intersectScissor :: V2 Float
                 -> V2 Float
                 -> VG ()
intersectScissor (V2 x y) (V2 w h) = 
    applyContext $ \ptr -> c_intersectScissor 
                                ptr 
                                (realToFrac x) (realToFrac y)
                                (realToFrac w) (realToFrac h)

resetScissor :: VG ()
resetScissor = applyContext c_resetScissor
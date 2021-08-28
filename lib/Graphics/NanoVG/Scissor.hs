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


-- | Sets the clipping region.
scissor :: V2 Float -- ^ position of top-left corner of clipping region
        -> V2 Float -- ^ dimentions of clipping region (V2 width height)
        -> VG ()
scissor (V2 x y) (V2 w h) = 
    applyContext $ \ptr -> c_scissor 
                                ptr 
                                (realToFrac x) (realToFrac y)
                                (realToFrac w) (realToFrac h)

-- | Sets clipping region to the intersection of the current clipping region and the region provided as an argument
intersectScissor :: V2 Float -- ^ position of top-left corner of region to insersect with clipping region
                 -> V2 Float -- ^ dimensions of region to intersect with clipping region
                 -> VG ()
intersectScissor (V2 x y) (V2 w h) = 
    applyContext $ \ptr -> c_intersectScissor 
                                ptr 
                                (realToFrac x) (realToFrac y)
                                (realToFrac w) (realToFrac h)

-- | After a call to this function, clipping is disabled.
resetScissor :: VG ()
resetScissor = applyContext c_resetScissor
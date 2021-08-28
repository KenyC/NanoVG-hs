{-|
Module      : Graphics.NanoVG.Path
Description : NanoVG instructions for creating paths.
Copyright   : (c) Keny C, 2021
License     : MIT
Stability   : experimental

To draw something, you need to give NanoVG a path (i.e. a list of lines or strokes) and then a method for rendering these segments. Here is a typical workflow:

@
-- First draw a rectangular path
withPath Open $ do
    rect (V2 0 0) (V2 100 200)

-- Then render it
fillColor $ fromRGB 255 0 0
fill
strokeColor $ fromRGB 0 255 0
stroke
@

This module defines the functions for creating a path, including 'withPath' and 'rect'.

Every path instruction for the current path ought to be in the scope 'withPath'. Basic paths can be created via a series of calls to 'moveTo' (displaces current position) and 'lineTo' (adds the line between current position and new position, updates current position). 
More complex shapes can be created with 'circle', 'rect', 'roundedRect', etc.

For functions that can draw or fill the path once it is created, see 'Graphics.NanoVG.Draw'. 
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.NanoVG.Path where

import Linear.V2
import Linear.V4

import Graphics.NanoVG.Internal.Path
import Graphics.NanoVG.Internal.Flag
import Graphics.NanoVG.Context

data PathType = Open    -- ^ final edge does not connect to first edge
              | Closed  -- ^ final edge connects to first edge
              deriving (Eq, Show)
            
-- | Starts new path. A path is a series of line, which can then be stroked (cf 'stroke') or filled (cf 'fill').
withPath :: PathType   -- ^ Whether the path should be closed
         -> VG ()      -- ^ Path drawing instructions
         -> VG ()
withPath closed cont = do
                            applyContext c_beginPath
                            cont
                            case closed of 
                                Open   -> return ()
                                Closed -> applyContext c_closePath




data PathWinding = Solid
                 | Hole
                 deriving (Eq, Show)

instance Flag PathWinding where
    toCInt Solid = _solid
    toCInt Hole  = _hole

pathWinding :: PathWinding -> VG ()
pathWinding winding = applyContext $ \ptr -> c_pathWinding ptr (toCInt winding)


-- | Adds a line from current position to the position given as argument to the current path.
lineTo :: V2 Float -- ^ Position to draw line
       -> VG ()
lineTo (V2 x y) = 
    applyContext $ \ptr -> c_lineTo 
                                ptr 
                                (realToFrac x)
                                (realToFrac y)

-- | Moves current position to position given as argument. No line is drawn.
moveTo :: V2 Float -- ^ New current position
       -> VG ()
moveTo (V2 x y) = 
    applyContext $ \ptr -> c_moveTo 
                              ptr 
                              (realToFrac x)
                              (realToFrac y)

-- | Adds a Bezier curve between current position and position given as argument to the path.
bezierTo :: V2 Float -- ^ control point 1
         -> V2 Float -- ^ control point 2
         -> V2 Float -- ^ position
         -> VG ()
bezierTo 
    (V2 cx1 cy1) 
    (V2 cx2 cy2) 
    (V2 x y) = 
    applyContext $ \ptr -> c_bezierTo  
                                ptr 
                                (realToFrac cx1) (realToFrac cy1)
                                (realToFrac cx2) (realToFrac cy2)
                                (realToFrac x)   (realToFrac y)


-- | Adds a part of a circle to the path.
arcTo :: V2 Float -- ^ corner 1
      -> V2 Float -- ^ corner 2
      -> Float    -- ^ radius
      -> VG ()
arcTo 
    (V2 cx1 cy1) 
    (V2 cx2 cy2) 
    radius = 
    applyContext $ \ptr -> c_arcTo  
                                ptr 
                                (realToFrac cx1) (realToFrac cy1)
                                (realToFrac cx2) (realToFrac cy2)
                                (realToFrac radius)

-- | Adds a quadratic Bezier curve between current position and position given as argument.
quadTo :: V2 Float -- ^ control point
       -> V2 Float -- ^ position
       -> VG ()
quadTo 
    (V2 cx1 cy1) 
    (V2 cx2 cy2) = 
    applyContext $ \ptr -> c_quadTo  
                                ptr 
                                (realToFrac cx1) (realToFrac cy1)
                                (realToFrac cx2) (realToFrac cy2)

-- | Adds a rectangle to the current path
rect :: V2 Float -- ^ position of top-left corner
     -> V2 Float -- ^ dimensions (width, height)
     -> VG ()
rect
    (V2 x y)
    (V2 w h) =
    applyContext $ \ptr -> 
        c_rect ptr 
            (realToFrac x) (realToFrac y) 
            (realToFrac w) (realToFrac h)

-- | Adds a rounded rectangle to the current path.
roundedRect :: V2 Float -- ^ position of top-left corner
            -> V2 Float -- ^ dimensions (width, height)
            -> Float    -- ^ radius of corner
            -> VG ()
roundedRect
    (V2 x y)
    (V2 w h)
    radius  =
    applyContext $ \ptr -> 
        c_roundedRect ptr 
            (realToFrac x) (realToFrac y) 
            (realToFrac w) (realToFrac h)
            (realToFrac radius)



-- | Adds a rounded rectangle with different radii for each corner to the current path.
roundedRectVarying :: V2 Float -- ^ position of top-left corner
                   -> V2 Float -- ^ dimensions (width, height)
                   -> V4 Float -- ^ radius of corners in the order: top left, top right, bottom right, bottom left
                   -> VG ()
roundedRectVarying
    (V2 x y)
    (V2 w h)
    (V4 tl tr br bl)  =
    applyContext $ \ptr -> 
        c_roundedRectVarying ptr 
            (realToFrac x) (realToFrac y) 
            (realToFrac w) (realToFrac h)
            (realToFrac tl) (realToFrac tr) (realToFrac br) (realToFrac bl) 

-- | Adds arc to current path.
arc :: V2 Float -- ^ position of arc center
    -> Float    -- ^ radius of circle
    -> Float    -- ^ initial angle
    -> Float    -- ^ final angle
    -> Bool     -- ^ true if clockwise
    -> VG ()
arc
    (V2 centerX centerY)
    radius
    initialAngle
    finalAngle
    dir
    =
    applyContext $ \ptr -> 
        c_arc ptr 
            (realToFrac centerX) (realToFrac centerY) 
            (realToFrac radius)
            (realToFrac initialAngle) (realToFrac finalAngle)
            (if dir then _clockwise else _counterClockwise)

-- | Adds arc to current path.
ellipse :: V2 Float -- ^ position of center
        -> Float    -- ^ radius on X
        -> Float    -- ^ radius on Y
        -> VG ()
ellipse
    (V2 centerX centerY)
    radiusX
    radiusY
    =
    applyContext $ \ptr -> 
        c_ellipse ptr 
            (realToFrac centerX) (realToFrac centerY) 
            (realToFrac radiusX) (realToFrac radiusY) 

-- | Adds arc to current path.
circle :: V2 Float -- ^ position of center
       -> Float    -- ^ radius 
       -> VG ()
circle
    (V2 centerX centerY)
    radius
    =
    applyContext $ \ptr -> 
        c_circle ptr 
            (realToFrac centerX) (realToFrac centerY) 
            (realToFrac radius)

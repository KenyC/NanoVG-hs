{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.NanoVG.Path where

import Control.Monad
import Linear.V2
import Foreign.ForeignPtr

import Graphics.NanoVG.Internal (c_rect)
import Graphics.NanoVG.Internal.Path
import Graphics.NanoVG.Context


-- | Starts new path. A path is a series of line, which can then be stroked (cf 'stroke') or filled (cf 'fill).
withPath :: Bool   -- ^ Whether the path should be closed
         -> VG ()  -- ^ Path drawing instructions
         -> VG ()
withPath closed cont = do
                            applyContext c_beginPath
                            cont
                            when closed $ applyContext c_closePath






-- | Adds a line from current position to the position given as argument to the current path 
lineTo :: V2 Float -- ^ Position to draw line
       -> VG ()
lineTo (V2 x y) = 
    applyContext $ \ptr -> c_lineTo 
                                ptr 
                                (realToFrac x)
                                (realToFrac y)

-- | Moves current position to position given as argument. No line is drawn
moveTo :: V2 Float -- ^ New current position
       -> VG ()
moveTo (V2 x y) = 
    applyContext $ \ptr -> c_moveTo 
                              ptr 
                              (realToFrac x)
                              (realToFrac y)

-- | Adds a Bezier curve between current position and position given as argument to the path
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


-- | Adds a part of a circle to the path
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
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.NanoVG.Path where

import Control.Monad
import Linear.V2
import Foreign.ForeignPtr

import Graphics.NanoVG.Internal (c_rect)
import Graphics.NanoVG.Internal.Path
import Graphics.NanoVG.Context

withPath :: Bool 
         -> VG () 
         -> VG ()
withPath closed cont = do
                            applyContext c_beginPath
                            cont
                            when closed $ applyContext c_closePath
        -- when closed cont
        -- when closed (applyContext c_closePath)






lineTo :: V2 Float
       -> VG ()
lineTo (V2 x y) = 
    applyContext $ \ptr -> c_lineTo 
                                ptr 
                                (realToFrac x)
                                (realToFrac y)

moveTo :: V2 Float
       -> VG ()
moveTo (V2 x y) = 
    applyContext $ \ptr -> c_moveTo 
                              ptr 
                              (realToFrac x)
                              (realToFrac y)

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

-- | Creates a rectangular path
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
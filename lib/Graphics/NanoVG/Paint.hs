{-|
Module      : Graphics.NanoVG.Paint
Description : Gradients and color patterns.
Copyright   : (c) Keny C, 2021
License     : MIT
Stability   : experimental

This module allow you to initialize and use 'Paint's. 'Paint's allow you to 'fill' paths with non-uniform, gradient and image color patterns.
Three gradient 'Paint's are defined: 'linearGradient', 'radialGradient', 'boxGradient'. Once created, the 'Paint' can be used with 'fillPaint'.
-}
module Graphics.NanoVG.Paint where

import Linear.V2
--
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import Graphics.NanoVG.Context
import Graphics.NanoVG.Color
import Graphics.NanoVG.Internal.Paint

data Paint = Paint {
    _getPaint :: !(ForeignPtr ())
}

createPaintFromPtr :: Ptr () -> IO Paint
createPaintFromPtr ptr = Paint <$> newForeignPtr finalizerFree ptr

-- | Sets paint to be used for stroke.
strokePaint :: Paint -> VG ()
strokePaint (Paint foreignPtr) = 
    applyContext $ \ptrContext -> 
    withForeignPtr foreignPtr $ \ptrPaint -> do
        c_strokePaint ptrContext ptrPaint


-- | Sets paint to be used for fill.
fillPaint :: Paint -> VG ()
fillPaint (Paint foreignPtr) = 
    applyContext $ \ptrContext -> 
    withForeignPtr foreignPtr $ \ptrPaint -> do
        c_fillPaint ptrContext ptrPaint


-- | Creates linear gradient from start point to end point.
linearGradient :: (V2 Float)  -- ^ position of start point
               -> (V2 Float)  -- ^ position of end point
               -> Color       -- ^ color at start point
               -> Color       -- ^ color at end point
               -> VG Paint
linearGradient 
    (V2 startX startY) 
    (V2 endX   endY)
    (Color startRed startGreen startBlue startAlpha)
    (Color endRed   endGreen   endBlue   endAlpha) = applyContext $ \ptr -> do
    createPaintFromPtr =<< c_linearGradient 
                               ptr
                               (realToFrac startX)   (realToFrac startY)
                               (realToFrac endX)     (realToFrac endY)
                               (realToFrac startRed) (realToFrac startGreen) (realToFrac startBlue) (realToFrac startAlpha)
                               (realToFrac endRed)   (realToFrac endGreen)   (realToFrac endBlue)   (realToFrac endAlpha)




-- | Creates paint which is a box gradient. A box gradient is a feathered (i.e. blurry) rounded rectangle, it is useful for rendering
--   drop shadows or highlights for boxes.
boxGradient :: (V2 Float)  -- ^ position of top left corner 
            -> (V2 Float)  -- ^ box dimensions (width x height)
            -> Float       -- ^ radius  of rounded corner
            -> Float       -- ^ feather, how blurry the rounded corner looks (in px) 
            -> Color       -- ^ color inside the box 
            -> Color       -- ^ color outside the box
            -> VG Paint
boxGradient 
    (V2 startX startY) 
    (V2 width  height)
    radius 
    feather
    (Color startRed startGreen startBlue startAlpha)
    (Color endRed   endGreen   endBlue   endAlpha) = applyContext $ \ptr -> do
    createPaintFromPtr =<< c_boxGradient 
                               ptr
                               (realToFrac startX)   (realToFrac startY)
                               (realToFrac width)    (realToFrac height)
                               (realToFrac radius)   (realToFrac feather)
                               (realToFrac startRed) (realToFrac startGreen) (realToFrac startBlue) (realToFrac startAlpha)
                               (realToFrac endRed)   (realToFrac endGreen)   (realToFrac endBlue)   (realToFrac endAlpha)




-- | Creates radial gradient from inner radius to outer radius.
radialGradient :: (V2 Float)  -- ^ position of gradient's center 
               -> Float       -- ^ inner radius
               -> Float       -- ^ outer radius 
               -> Color       -- ^ color inside inner radius 
               -> Color       -- ^ color outside outer radius
               -> VG Paint
radialGradient 
    (V2 centerX centerY) 
    innerRadius 
    outerRadius
    (Color startRed startGreen startBlue startAlpha)
    (Color endRed   endGreen   endBlue   endAlpha) = applyContext $ \ptr -> do
    createPaintFromPtr =<< c_radialGradient 
                               ptr
                               (realToFrac centerX)     (realToFrac centerY)
                               (realToFrac innerRadius) (realToFrac outerRadius)
                               (realToFrac startRed) (realToFrac startGreen) (realToFrac startBlue) (realToFrac startAlpha)
                               (realToFrac endRed)   (realToFrac endGreen)   (realToFrac endBlue)   (realToFrac endAlpha)





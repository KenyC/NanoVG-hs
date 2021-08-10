module Graphics.NanoVG.Transform where

import Foreign.Marshal.Array
--
import Linear.V2
import Linear.V3
import Linear.Matrix

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.Transform


putTransform :: M33 Float -> VG ()
putTransform matrix = applyContext $ \ptr -> c_transform ptr a b c d e f
                      where (V3 (V3 a c e) (V3 b d f) _) = fmap realToFrac <$> matrix


getTransform :: VG (M33 Float)
getTransform = applyContext $ \ptr -> do
    withArray [0 | _ <- [1 .. 6]] $ \ptrTf -> do
        c_currentTransform ptr ptrTf 
        a:b:c:d:e:f:[]  <- map realToFrac <$> peekArray 6 ptrTf
        return $ V3
                    (V3 a c e)
                    (V3 b d f)
                    (V3 0 0 1)


resetTransform :: VG ()
resetTransform = applyContext c_resetTransform

translate :: V2 Float 
          -> VG ()
translate (V2 x y) = applyContext $ \ptr -> c_translate 
                                                 ptr 
                                                 (realToFrac x) 
                                                 (realToFrac y)

rotate :: Float 
       -> VG ()
rotate angle = applyContext $ \ptr -> c_rotate ptr (realToFrac angle)

skewX :: Float 
      -> VG ()
skewX value = applyContext $ \ptr -> c_skewX ptr (realToFrac value)

skewY :: Float 
      -> VG ()
skewY value = applyContext $ \ptr -> c_skewY ptr (realToFrac value)


scale :: V2 Float 
      -> VG ()
scale (V2 sx sy) = applyContext $ \ptr -> c_scale ptr (realToFrac sx) (realToFrac sy)




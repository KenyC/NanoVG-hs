{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.NanoVG.Transform where

import Foreign.Marshal.Array
--
import Linear.V2 hiding (angle)
import Linear.V3
import Linear.Matrix

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal.Transform

-- | Sets current tranform with a 3x3 matrix. The matrix must be in the following form
--   \[
--   \begin{matrix}{ccc}
--   a & b & c \\
--   d & e & f \\
--   0 & 0 & 1
--   \end{matrix}
--   \]
putTransform :: M33 Float -> VG ()
putTransform matrix = applyContext $ \ptr -> c_transform ptr a b c d e f
                      where (V3 (V3 a c e) (V3 b d f) _) = fmap realToFrac <$> matrix


-- | Get transform as a 3x3 matrix.
getTransform :: VG (M33 Float)
getTransform = applyContext $ \ptr -> do
    withArray [0 | _ :: Int <- [1 .. 6]] $ \ptrTf -> do
        c_currentTransform ptr ptrTf 
        a:b:c:d:e:f:[]  <- map realToFrac <$> peekArray 6 ptrTf
        return $ V3
                    (V3 a c e)
                    (V3 b d f)
                    (V3 0 0 1)

-- | Reset transform to identity matrix
resetTransform :: VG ()
resetTransform = applyContext c_resetTransform

-- | Translate origin by vector
translate :: V2 Float 
          -> VG ()
translate (V2 x y) = applyContext $ \ptr -> c_translate 
                                                 ptr 
                                                 (realToFrac x) 
                                                 (realToFrac y)

-- | Rotate axes by angle (in radians)
rotate :: Float 
       -> VG ()
rotate angle = applyContext $ \ptr -> c_rotate ptr (realToFrac angle)

skewX :: Float 
      -> VG ()
skewX value = applyContext $ \ptr -> c_skewX ptr (realToFrac value)

skewY :: Float 
      -> VG ()
skewY value = applyContext $ \ptr -> c_skewY ptr (realToFrac value)


-- | Scale X and Y axis by value
scale :: V2 Float -- (x scaling factor, y scaling factor)
      -> VG ()
scale (V2 sx sy) = applyContext $ \ptr -> c_scale ptr (realToFrac sx) (realToFrac sy)




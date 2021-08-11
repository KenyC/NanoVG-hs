module Graphics.NanoVG.Image (
    Image(),
    withImage,
    imagePattern
) where

import Control.Exception (bracket)
--
import Linear.V2
--
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types

import Graphics.NanoVG.Context
import Graphics.NanoVG.Paint
import Graphics.NanoVG.Internal.Image
import Graphics.NanoVG.Internal.Paint

data Image = Image {
    _imageHandle :: !CInt
}


withImage :: NVGContext
          -> FilePath 
          -> (Maybe Image -> IO a)
          -> IO a
withImage (NVGContext context) pathToImage = 
    bracket before after
    where before = withForeignPtr context $ \ptr -> do
                handle <- withCString pathToImage $ \c_pathToImage ->
                    c_createImage ptr c_pathToImage 0
                case handle of 
                    0 -> return Nothing
                    _ -> return $ Just $ Image handle

          after Nothing               = return ()
          after (Just (Image handle)) = withForeignPtr context $ \ptr -> c_deleteImage ptr handle


imagePattern :: (V2 Float)
             -> (V2 Float)
             -> Float
             -> Float
             -> Image
             -> VG Paint
imagePattern
    (V2 startX endX)
    (V2 startY endY)
    angle
    alpha
    (Image handle) = applyContext $ \ptr -> do
        createPaintFromPtr =<< c_imagePattern 
                                    ptr
                                    (realToFrac startX) (realToFrac endX)
                                    (realToFrac startY) (realToFrac endY)
                                    (realToFrac angle)
                                    handle
                                    (realToFrac alpha)


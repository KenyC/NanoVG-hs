{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.NanoVG.Image (
    Image(),
    withImage,
    imagePattern,
    imageSize
) where

import Control.Exception (bracket)
--
import Linear.V2
--
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

import Graphics.NanoVG.Context
import Graphics.NanoVG.Paint
import Graphics.NanoVG.Internal.Image
import Graphics.NanoVG.Internal.Paint

data Image = Image {
    _imageHandle :: !CInt
}

-- | Runs the provided computation with an 'Image' loaded from file (extensions accepted: jpg, png, psd, tga, pic and gif).
--   If image file does not exist or cannot be loaded, Nothing is passed to the continuation.
withImage :: NVGContext -- ^ NanoVG context
          -> FilePath   -- ^ Path to image file
          -> (Maybe Image -> IO a) -- ^
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


-- | Returns dimensions (width, height) of image.
imageSize :: Image -> VG (V2 Int)
imageSize (Image image) = applyContext $ \ptr -> do
    alloca $ \(c_width  :: Ptr CInt) ->
        alloca $ \(c_height :: Ptr CInt) -> do
            c_imageSize ptr image c_width c_height
            width  <- peek c_width
            height <- peek c_height
            return $! fromIntegral  <$> V2 width height


-- | Creates a texture from an image. The texture can then be passed to 'fillPaint' to draw the image on the screen.
imagePattern :: (V2 Float) -- ^ Where to draw top left corner of image 
             -> (V2 Float) -- ^ Where to draw bottom right of image
             -> Float      -- ^ Angle of rotation of image
             -> Float      -- ^ Level of transparency of image (i.e. alpha value, from 0 to 1)
             -> Image      -- ^ image
             -> VG Paint   -- ^ paint ; can be passed to 'fillPaint'
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


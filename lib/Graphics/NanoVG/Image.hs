{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.NanoVG.Image (
    Image(),
    ImageFlag(..),
    withImage,
    withImageRGBA,
    imagePattern,
    imageSize
) where

import Control.Exception (bracket)
--
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe
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
withImage :: NVGContext   -- ^ NanoVG context
          -> FilePath     -- ^ Path to image file
          -> [ImageFlag]  -- ^ Flags
          -> (Maybe Image -> IO a) -- ^ Computation to be run with image
          -> IO a
withImage (NVGContext context) pathToImage flags = 
    bracket before after
    where before = withForeignPtr context $ \ptr -> do
                handle <- withCString pathToImage $ \c_pathToImage ->
                    c_createImage ptr c_pathToImage (compileImageFlags flags)
                case handle of 
                    0 -> return Nothing
                    _ -> return $ Just $ Image handle

          after Nothing               = return ()
          after (Just (Image handle)) = withForeignPtr context $ \ptr -> c_deleteImage ptr handle


-- | Load an image from memory.
withImageRGBA :: NVGContext      -- ^ NanoVG context
              -> V2 Int          -- ^ Image dimensions (width, height)
              -> ByteString      -- ^ Image RGBA data. The layout of the data is column-first, channel-last: at index i*height*4 + j**4 + c, is found the *c*-th component of the pixel in row i and column j.
              -> [ImageFlag]     -- ^ Flags 
              -> (Image -> IO a) -- ^ Computation to be run with image
              -> IO a

withImageRGBA 
    (NVGContext context) 
    (V2 width height) 
    imgData
    flags
    = 
    bracket before after
    where before = do
                      withForeignPtr context        $ \ptr        -> do
                          unsafeUseAsCString imgData    $ \imgArray   -> do
                            Image <$> c_createImageRGBA 
                                ptr                        
                                (fromIntegral width)
                                (fromIntegral height)
                                (compileImageFlags flags)
                                (castPtr imgArray)

          after (Image handle) = withForeignPtr context $ \ptr -> c_deleteImage ptr handle


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


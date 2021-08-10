module Graphics.NanoVG.Internal.Image where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr



-- | Creates image by loading it from the disk from specified file name.
--   Returns handle to the image.
foreign import ccall unsafe "nanovg.h nvgCreateImage"
    c_createImage :: Ptr ()
                  -> Ptr CChar
                  -> CInt
                  -> IO CInt


-- | Creates image by loading it from the specified chunk of memory.
--   Returns handle to the image.
foreign import ccall unsafe "nanovg.h nvgCreateImage"
    c_createImageMem :: Ptr ()
                     -> CInt
                     -> Ptr CUChar
                     -> IO CInt

-- | Creates image from specified image data.
--   Returns handle to the image.
foreign import ccall unsafe "nanovg.h nvgCreateRGBA"
    c_createImageRGBA :: Ptr ()
                      -> CInt
                      -> CInt 
                      -> CInt
                      -> Ptr CUChar
                      -> IO CInt

-- | Updates image data specified by image handle.
foreign import ccall unsafe "nanovg.h nvgUpdateImage"
    c_updateImage :: Ptr ()
                 ->  CInt
                 ->  Ptr CUChar
                 ->  IO ()

-- | Returns the dimensions of a created image.
foreign import ccall unsafe "nanovg.h nvgImageSize"
    c_imageSize :: Ptr () 
                -> CInt
                -> Ptr CInt
                -> Ptr CInt
                -> IO ()


-- | Deletes created image.
foreign import ccall unsafe "nanovg.h nvgDeleteImage"
    c_deleteImage :: Ptr () -> CInt -> IO ()

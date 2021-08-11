module Graphics.NanoVG.Internal.Image where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
--
import Data.Bits

#include "nanovg.h"


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
foreign import ccall unsafe "nanovg.h nvgCreateImageRGBA"
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


data ImageFlag = GenerateMipmaps  -- ^ Generate mipmaps during creation of the image. 
                | RepeatX          -- ^ Repeat image in X direction.     
                | RepeatY          -- ^ Repeat image in Y direction.           
                | FlipY            -- ^ Flips (inverses) image in Y direction when rendered.         
                | Premultiplied    -- ^ Image data has premultiplied alpha.        
                | Nearest          -- ^ Image interpolation is Nearest instead Linear       



#{enum CInt,, 
    _imageflag_generate_mipmaps = NVG_IMAGE_GENERATE_MIPMAPS,
    _imageflag_repeatx          = NVG_IMAGE_REPEATX,
    _imageflag_repeaty          = NVG_IMAGE_REPEATY,
    _imageflag_flipy            = NVG_IMAGE_FLIPY,
    _imageflag_premultiplied    = NVG_IMAGE_PREMULTIPLIED,
    _imageflag_nearest          = NVG_IMAGE_NEAREST
}

compileImageFlags :: [ImageFlag] -> CInt
compileImageFlags = foldr ((.|.) . toIntVal)  0
                    where toIntVal :: ImageFlag -> CInt
                          toIntVal GenerateMipmaps = _imageflag_generate_mipmaps
                          toIntVal RepeatX         = _imageflag_repeatx
                          toIntVal RepeatY         = _imageflag_repeaty   
                          toIntVal FlipY           = _imageflag_flipy 
                          toIntVal Premultiplied   = _imageflag_premultiplied 
                          toIntVal Nearest         = _imageflag_nearest  
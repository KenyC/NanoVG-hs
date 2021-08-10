{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.NanoVG.Internal where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
--
import Data.Bits

#include "nanovg.h"
#include "nanovg_gl.h"
#include "nanovg_hs_wrapper.h"

foreign import ccall "nanovg_gl.h nvgCreateGL3"
    c_createGL3 :: CInt -> IO (Ptr ())

foreign import ccall "nanovg_gl.h &nvgDeleteGL3"
    c_destroyGL3 :: FinalizerPtr ()

-- foreign import ccall "nanovg_gl.h nvgDeleteGL3"
--     c_destroyGL :: Ptr () -> IO ()



foreign import ccall unsafe "nanovg.h nvgBeginFrame"
    c_beginFrame :: Ptr ()
                 -> CFloat
                 -> CFloat
                 -> CFloat
                 -> IO ()

foreign import ccall unsafe "nanovg.h nvgEndFrame"
    c_endFrame :: Ptr ()
               -> IO ()


------------------- DRAW FUNCTIONS -----------------

foreign import ccall unsafe "nanovg.h nvgRect"
    c_rect :: Ptr ()
           -> CFloat
           -> CFloat
           -> CFloat
           -> CFloat
           -> IO ()



foreign import ccall unsafe "nanovg.h nvgStroke"
    c_stroke :: Ptr ()
             -> IO ()
foreign import ccall unsafe "nanovg.h nvgFill"
    c_fill :: Ptr ()
           -> IO ()

------------------- STATE CHANGE -----------------

foreign import ccall unsafe "nanovg.h nvgFillColorHs"
    c_fillColor :: Ptr ()
                -> CFloat -- ^ red
                -> CFloat -- ^ green
                -> CFloat -- ^ blue
                -> CFloat -- ^ alpha
                -> IO ()

foreign import ccall unsafe "nanovg.h nvgStrokeColorHs"
    c_strokeColor :: Ptr ()
                  -> Float -- ^ red
                  -> Float -- ^ green
                  -> Float -- ^ blue
                  -> Float -- ^ alpha
                  -> IO ()


------------------- C to Haskell translation utils -----------------

newtype CreateFlags = CreateFlags { unCreateFlags :: CInt }
                      deriving (Eq,Show)

#{enum CreateFlags, CreateFlags
  , antialias             = NVG_ANTIALIAS
  , stencil_strokes       = NVG_STENCIL_STROKES
  , debug                 = NVG_DEBUG
}


-- | Combine a list of options into a single option, using bitwise (.|.)
compileCreateFlags :: [CreateFlags] -> CInt
compileCreateFlags = foldr ((.|.) . unCreateFlags) 0

------------------- C structs -----------------

-- data Color = Color {
--     _red   :: CFloat,
--     _green :: CFloat,
--     _blue  :: CFloat,
--     _alpha :: CFloat
-- }

-- instance Storable Color where
--   alignment _ = #{alignment NVGcolor}
--   sizeOf _ = #{size NVGcolor}
--   peek ptr = do
--     red   <- #{peek NVGcolor, r} ptr
--     green <- #{peek NVGcolor, g} ptr
--     blue  <- #{peek NVGcolor, b} ptr
--     alpha <- #{peek NVGcolor, a} ptr
--     return (Color red green blue alpha)

--   poke ptr (Color red green blue alpha) = do
--     #{poke NVGcolor, r} ptr red
--     #{poke NVGcolor, g} ptr green
--     #{poke NVGcolor, b} ptr blue
--     #{poke NVGcolor, a} ptr alpha
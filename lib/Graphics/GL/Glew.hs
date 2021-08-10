{-# LANGUAGE ForeignFunctionInterface #-}

module Glew where

import Graphics.GL
--
import Foreign
import Foreign.C.Types


foreign import ccall "GL/glew.h glewInit" 
    glewInit :: IO GLenum

 

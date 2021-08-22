module Graphics.NanoVG.Internal.Flag where

import Data.Bits
import Foreign.C.Types

class Flag a where
    toCInt :: a -> CInt

compileFlags :: (Flag a)
             => [a]
             -> CInt
compileFlags = foldr ((.|.) . toCInt)  0
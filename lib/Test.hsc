{-# LANGUAGE ForeignFunctionInterface #-}
module Test where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
--
import Data.Bits


foreign import ccall "testy_fun.h testy_fun"
    c_testyFun :: IO Int

foreign import ccall "testy_fun.h init_test_struct"
    c_initStruct :: IO (Ptr ())

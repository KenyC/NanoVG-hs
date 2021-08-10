{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.NanoVG.Context where

import Foreign.Ptr
import Foreign.ForeignPtr
--
import Linear.V2
--
import Control.Monad.Reader
import Control.Monad.IO.Class

import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.State

data NVGContext = NVGContext {
    _getNVGContext :: !(ForeignPtr ())
}

data WindowResolution = WindowResolution {
    _size :: !(V2 Float),
    _dpi  :: !Float
}

newtype VG a = VG {
    unwrapVG :: ReaderT NVGContext IO a
} deriving (Functor, Applicative, Monad, MonadIO)

withContext :: NVGContext 
            -> VG a 
            -> IO a
withContext context (VG body) = runReaderT body context


applyContext :: (Ptr () -> IO a) -> VG a
applyContext function = VG $ withReaderT _getNVGContext $ do
                            foreignPtr <- ask
                            lift $ withForeignPtr foreignPtr function


nvgGL3Context :: [CreateFlags] -> IO NVGContext
nvgGL3Context flags = do 
    pointer    <- c_createGL3 $ compileCreateFlags flags
    foreignPtr <- newForeignPtr c_destroyGL3 pointer
    return $ NVGContext foreignPtr



save :: VG ()
save = applyContext c_save

restore :: VG ()
restore = applyContext c_restore

reset :: VG ()
reset = applyContext c_reset
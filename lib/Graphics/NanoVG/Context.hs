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

-- | Monad for drawing instructions. 
--   Under the hood, a reader monad over IO, with NanoVG context as parameter
newtype VG a = VG {
    unwrapVG :: ReaderT NVGContext IO a
} deriving (Functor, Applicative, Monad, MonadIO)


-- | Performs vector graphics drawing in the given context
withContext :: NVGContext -- ^ NanoVG context
            -> VG a       -- ^ drawing instructions
            -> IO a
withContext context (VG body) = runReaderT body context


applyContext :: (Ptr () -> IO a) -> VG a
applyContext function = VG $ withReaderT _getNVGContext $ do
                            foreignPtr <- ask
                            liftIO $ withForeignPtr foreignPtr function


-- | Creates a NanoVG context for OpenGL 3
nvgGL3Context :: [CreateFlags] -> IO NVGContext
nvgGL3Context flags = do 
    pointer    <- c_createGL3 $ compileCreateFlags flags
    foreignPtr <- newForeignPtr c_destroyGL3 pointer
    return $ NVGContext foreignPtr


-- | Pushes current drawing state (i.e. stroke color, transform, fill paint, etc.) to a stack of states. 
--   Then sets current drawing state back to default values.
--   Used in combination with ``restore``
save :: VG ()
save = applyContext c_save

-- | Pops drawing state from stack (i.e. stroke color, transform, fill paint, etc.) and sets it to current state
--   Used in combination with ``save``
restore :: VG ()
restore = applyContext c_restore

-- | Executes all commands in scope argument in new default drawing state. Previous drawing state is restored upon exit of the scope argument.
withNewState :: VG a  -- ^ Scope to be executed in new default drawing state
             -> VG a
withNewState cont = do
    save
    toReturn <- cont
    restore
    return toReturn

-- | Resets drawing state to default initial values
reset :: VG ()
reset = applyContext c_reset
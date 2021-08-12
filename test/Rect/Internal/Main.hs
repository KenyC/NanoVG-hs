{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL (($=))
import qualified SDL
--
import Graphics.GL
--
import Control.Monad
import Control.Monad.IO.Class
--
import Foreign.ForeignPtr
--
import Linear.V2

import Graphics.NanoVG
import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal.Path  (c_rect)
import Graphics.NanoVG.Internal
import Glew



main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    
    let windowSize = V2 800 600
    let windowResolution = WindowResolution (realToFrac <$> windowSize) 4.0
    window <- SDL.createWindow "Test" $ SDL.defaultWindow {
        SDL.windowInitialSize     = windowSize,
        SDL.windowGraphicsContext = SDL.OpenGLContext $ SDL.defaultOpenGL {SDL.glProfile = SDL.Core SDL.Normal 3 3}
    }
    
    context <- SDL.glCreateContext window
    glewInit

  
    glClearColor 1 1 1 1

    nanovg <- nvgGL3Context [debug]

    let render = do
                glClear $ GL_COLOR_BUFFER_BIT
                frame nanovg windowResolution $ do
                        liftIO $ withForeignPtr (_getNVGContext nanovg) $ \ptr -> do
                            c_rect ptr 20 30 20 30
                            c_strokeColor ptr 1 0 0 1
                            c_stroke ptr

    let appLoop = do
            render
            SDL.glSwapWindow $ window
            events <- SDL.pollEvents
            SDL.delay 1000
            unless (any quitEvent events) appLoop

    appLoop

    SDL.destroyWindow window
    SDL.quit



quitEvent :: SDL.Event -> Bool
quitEvent event = case SDL.eventPayload event of 
    SDL.KeyboardEvent keyboardEvent -> SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                                       SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    SDL.QuitEvent -> True
    _         -> False
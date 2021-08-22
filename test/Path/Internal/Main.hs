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
import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.Draw
import Graphics.NanoVG.Internal.Path
import Glew
import Test



main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    
    let windowSize = V2 300 200
    let windowResolution = WindowResolution (realToFrac <$> windowSize) 4.0
    window <- SDL.createWindow "Test" $ SDL.defaultWindow {
        SDL.windowInitialSize     = windowSize,
        SDL.windowGraphicsContext = SDL.OpenGLContext $ SDL.defaultOpenGL {SDL.glProfile = SDL.Core SDL.Normal 3 3}
    }
    
    context <- SDL.glCreateContext window
    glewInit

  
    glClearColor 1 1 1 1

    nanovg <- nvgGL3Context [Debug]

    let render = do
                glClear $ GL_COLOR_BUFFER_BIT
                frame nanovg windowResolution $ do
                        liftIO $ withForeignPtr (_getNVGContext nanovg) $ \ptr -> do
                            c_beginPath ptr
                            c_moveTo ptr 20 20
                            c_lineTo ptr 40 40
                            c_lineTo ptr 40 60
                            c_bezierTo ptr 40 80 60 80 60 60
                            c_arcTo    ptr 80 60 90 70 20
                            c_quadTo   ptr 100 100 140 120
                            c_strokeColor ptr 1 0 0 2
                            c_closePath ptr
                            c_stroke ptr

    let appLoop = do
            render
            SDL.glSwapWindow $ window
            events <- SDL.pollEvents
            SDL.delay 1
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
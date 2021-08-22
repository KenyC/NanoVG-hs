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
import Foreign.Marshal.Alloc
--
import Linear.V2

import Graphics.NanoVG
import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.Draw
import Graphics.NanoVG.Internal.Paint
import Graphics.NanoVG.Internal.Path
import Glew
import Test



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

    nanovg <- nvgGL3Context [Debug]



    let render = do
                glClear $ GL_COLOR_BUFFER_BIT
                frame nanovg windowResolution $ do
                        liftIO $ withForeignPtr (_getNVGContext nanovg) $ \ptr -> do

                            -- c_printPaint linear
                            c_beginPath ptr
                            linear <- c_linearGradient ptr
                                        0 0
                                        100 0
                                        1 0 0 1
                                        0 1 0 1
                            c_rect ptr 0 0 200 50
                            c_fillPaint ptr linear
                            c_fill ptr
                            free linear

                            c_beginPath ptr
                            box <- c_boxGradient ptr
                                        0 100
                                        200 50
                                        20 5
                                        1 0 0 1
                                        0 1 0 1
                            c_rect ptr 0 100 200 50
                            c_fillPaint ptr box
                            c_fill ptr
                            free box

                            c_beginPath ptr
                            radial <- c_radialGradient ptr
                                        100 300
                                        10 150
                                        1 0 0 1
                                        0 1 0 1
                            c_rect ptr 0 200 200 200
                            c_fillPaint ptr radial
                            c_fill ptr
                            free radial



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
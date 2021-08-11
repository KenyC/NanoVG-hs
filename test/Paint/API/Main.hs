{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL (($=))
import qualified SDL
--
import Graphics.GL
--
import Control.Monad
--
import Foreign.ForeignPtr
--
import Linear.V2

import Graphics.NanoVG
import Graphics.NanoVG.Color
import Graphics.NanoVG.Context
import Graphics.NanoVG.Draw
import Graphics.NanoVG.Paint
import Graphics.NanoVG.Path
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

    nanovg <- nvgGL3Context [debug]
    linear <- withContext nanovg $ linearGradient
                                (V2 0 0)
                                (V2 100 0)
                                (Color 1 0 0 1)
                                (Color 0 1 0 1)

    box <- withContext nanovg $ boxGradient
                                (V2 0 100)
                                (V2 200 50)
                                20
                                5
                                (Color 1 0 0 1)
                                (Color 0 1 0 1)

    radial <- withContext nanovg $ radialGradient
                                (V2 100 300)
                                10 
                                150
                                (Color 1 0 0 1)
                                (Color 0 1 0 1)

    let render = do
                glClear $ GL_COLOR_BUFFER_BIT

                frame nanovg windowResolution $ do

                    withPath False $ do
                        rect 0 (V2 200 50)
                        fillPaint linear
                        fill   

                    withPath False $ do
                        rect (V2 0 100) (V2 200 50)
                        fillPaint box
                        fill  

                    withPath False $ do
                        rect (V2 0 200) (V2 200 200)
                        fillPaint radial
                        fill     



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
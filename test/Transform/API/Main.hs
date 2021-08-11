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
import Linear.V3

import Graphics.NanoVG
import Graphics.NanoVG.Color
import Graphics.NanoVG.Context
import Graphics.NanoVG.Transform
import Graphics.NanoVG.Draw
import Graphics.NanoVG.Path
import Glew
import Test



main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    
    let windowSize = V2 200 300
    let windowResolution = WindowResolution (realToFrac <$> windowSize) 4.0
    window <- SDL.createWindow "Test" $ SDL.defaultWindow {
        SDL.windowInitialSize     = windowSize,
        SDL.windowGraphicsContext = SDL.OpenGLContext $ SDL.defaultOpenGL {SDL.glProfile = SDL.Core SDL.Normal 3 3}
    }
    
    context <- SDL.glCreateContext window
    glewInit

  
    glClearColor 1 1 1 1

    nanovg <- nvgGL3Context [debug]

    let square :: VG ()
        square = withPath False $ do
            rect (V2 0 0) (V2 20 20)
            strokeColor (Color 1 0 0 1)
            stroke
            return ()

    let render = do
                glClear $ GL_COLOR_BUFFER_BIT
                frame nanovg windowResolution $ do
                        square

                        translate $ V2 50 50
                        square

                        rotate (pi/4)
                        square
                        save

                        resetTransform
                        translate $ V2 40 30
                        square

                        resetTransform
                        translate $ V2 80 80
                        skewX 2
                        square

                        resetTransform
                        translate $ V2 80 80
                        skewY 2
                        square

                        resetTransform 
                        tf <- getTransform
                        liftIO $ print tf

                        let tf = V3
                                    (V3 2 0 2)
                                    (V3 0 3 4)
                                    (V3 0 0 1)
                        putTransform tf
                        square

                        restore
                        translate $ V2 5 5
                        square

                        reset
                        translate $ V2 5 5
                        square

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
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
import Foreign.C.String
--
import Linear.V2

import Graphics.NanoVG
import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.Paint
import Graphics.NanoVG.Internal.Path  (c_rect)
import Graphics.NanoVG.Internal.Image
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
    handle <- withForeignPtr (_getNVGContext nanovg) $ \ptr -> 
                withCString "resources/image.jpg" $ \filename ->
                    c_createImage ptr filename 0


    let render = do
                    glClear $ GL_COLOR_BUFFER_BIT
                    frame nanovg windowResolution $ do
                        liftIO $ withForeignPtr (_getNVGContext nanovg) $ \ptr -> do
                            imagePattern <- c_imagePattern ptr
                                        0 0
                                        100 100
                                        0
                                        handle
                                        1

                            c_rect ptr 0 0 50 50
                            c_fillPaint ptr imagePattern
                            c_fill ptr
                            free imagePattern

    let appLoop = do
            render
            SDL.glSwapWindow $ window
            events <- SDL.pollEvents
            SDL.delay 1000
            unless (any quitEvent events) appLoop

    appLoop
    withForeignPtr (_getNVGContext nanovg) $ \ptr -> c_deleteImage ptr handle

    SDL.destroyWindow window
    SDL.quit



quitEvent :: SDL.Event -> Bool
quitEvent event = case SDL.eventPayload event of 
    SDL.KeyboardEvent keyboardEvent -> SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                                       SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    SDL.QuitEvent -> True
    _         -> False
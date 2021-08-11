{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
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
import Graphics.NanoVG.Path
import Graphics.NanoVG.Image
import Graphics.NanoVG.Paint
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

    withImage nanovg "resources/image.jpg" [FlipY] $ \case
        Nothing    -> putStrLn "Couldn't load image... Exiting!" 
        Just image -> do
                size       <- withContext nanovg $ imageSize image
                imagePaint <- withContext nanovg $ 
                                imagePattern 
                                    0 (fromIntegral <$> size) 
                                    0 1 
                                    image

                let render = do
                        glClear $ GL_COLOR_BUFFER_BIT
                        frame nanovg windowResolution $ do
                                rect (V2 0 0) (V2 50 50)
                                fillPaint imagePaint
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
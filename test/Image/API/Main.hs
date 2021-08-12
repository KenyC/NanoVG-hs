{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
module Main where

import Data.ByteString  (ByteString)
import qualified Data.ByteString as BS
--
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
import Graphics.NanoVG.Transform
import Graphics.NanoVG.Paint
import Glew
import Test


-- A radial gradient from black to green
imageData :: ByteString
imageData = BS.pack $ [ case (i, j, k) of 
                            (_, _, 3)   -> 255
                            (0, j, 1)   -> fromIntegral $ min 255 $ ((i - 32) * (i - 32) + (j - 32) * (j - 32) :: Int)
                            (_, _, _)   -> 0
                      | i <- [0 .. 63] , 
                        j <- [0 .. 63] ,
                        k <- [0 .. 3]  
                      ]


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
        Just image -> withImageRGBA nanovg (V2 64 64) imageData [] $ \image2 -> do
                size1       <- withContext nanovg $ imageSize image
                size2       <- withContext nanovg $ imageSize image2
                imagePaint <- withContext nanovg $ 
                                imagePattern 
                                    0 (fromIntegral <$> size2) 
                                    0 1 
                                    image

                image2Paint <- withContext nanovg $ 
                                    imagePattern 
                                        0 (fromIntegral <$> size2) 
                                        0 1 
                                        image2

                let render = do
                        glClear GL_COLOR_BUFFER_BIT
                        frame nanovg windowResolution $ do
                                withPath False $ do
                                    rect (V2 0 0) (V2 50 50)
                                    fillPaint imagePaint
                                    fill     

                                withPath False $ do
                                    translate $ V2 50 50
                                    rect (V2 0 0) $ fromIntegral <$> size2
                                    fillPaint image2Paint
                                    -- fillColor $ Color 1 0 0 1
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
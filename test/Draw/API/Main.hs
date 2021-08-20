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
import Graphics.NanoVG.Scissor
import Graphics.NanoVG.Transform
import Graphics.NanoVG.Paint
import Glew
import Test




main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    
    let windowSize = V2 1000 800
    let windowResolution = WindowResolution (realToFrac <$> windowSize) 4.0
    window <- SDL.createWindow "Test" $ SDL.defaultWindow {
        SDL.windowInitialSize     = windowSize,
        SDL.windowGraphicsContext = SDL.OpenGLContext $ SDL.defaultOpenGL {SDL.glProfile = SDL.Core SDL.Normal 3 3}
    }
    
    context <- SDL.glCreateContext window
    glewInit

  
    glClearColor 1 1 1 1

    nanovg <- nvgGL3Context [debug]

    let draw2Rects = do
                        withPath False $ do
                            rect 0 50
                            fillColor $ fromRGBA 255 0 0 255
                            fill

                        withPath False $ do
                            rect 25 50
                            fillColor $ fromRGBA 0 255 0 255
                            fill

    let render = do
            glClear GL_COLOR_BUFFER_BIT
            frame nanovg windowResolution $ do
                drawLines

                translate $ V2 800 500
                scissor 0 $ V2 20 100
                withPath False $ circle 0 50
                fillColor $ Color 1 0 0 1
                fill

                intersectScissor 10 $ V2 20 100
                withPath False $ circle 0 50
                fillColor $ Color 0 1 0 1
                fill

                resetScissor
                withPath False $ circle 0 20
                fillColor $ Color 0 0 1 1
                fill


                -- draw2Rects    

                -- translate $ V2 100 0
                -- globalCompositeOperation SourceAtop
                -- draw2Rects    


                -- withPath False $ do
                --     rect 0 100
                --     fillColor $ fromRGBA 255 0 255 0
                --     fill
                -- translate $ V2 100 0
                -- globalCompositeOperation DestinationOut
                -- -- globalCompositeOperation Xor
                -- draw2Rects  



    let appLoop = do
            render
            SDL.glSwapWindow $ window
            events <- SDL.pollEvents
            SDL.delay 1000
            unless (any quitEvent events) appLoop

    appLoop

    SDL.destroyWindow window
    SDL.quit


drawLines :: VG ()
drawLines = do

    let enumerate = zip [0..]
        points    = [ V2 10 20
                    , V2 30 40
                    , V2 5  35 ]

    forM_ (enumerate [Miter, RoundJoin, Bevel]) $ \(i, joinStyle) -> 
        forM_ (enumerate [Butt, RoundCap, Square]) $ \(j, capStyle) -> 
            withNewState $ do
                scale 4
                translate $ 50 * V2 i j 
                translate 20
                lineCap    capStyle
                lineJoin   joinStyle

                strokeWidth 3
                strokeColor $ fromRGBA 0 0 0 160

                withPath False $ do
                    moveTo 0
                    mapM_ lineTo points 
                    stroke

                strokeWidth 0.25
                strokeColor $ fromRGBA 0 192 255 255

                withPath False $ do
                    moveTo 0
                    mapM_ lineTo points 
                    stroke

    return ()

quitEvent :: SDL.Event -> Bool
quitEvent event = case SDL.eventPayload event of 
    SDL.KeyboardEvent keyboardEvent -> SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                                       SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    SDL.QuitEvent -> True
    _         -> False
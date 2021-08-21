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
--
import Data.Text (Text)

import Graphics.NanoVG
import Graphics.NanoVG.Color
import Graphics.NanoVG.Context
import Graphics.NanoVG.Text
import Graphics.NanoVG.Draw
import Graphics.NanoVG.Path
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

    nanovg     <- nvgGL3Context [debug]
    Just robotoFont <- withContext nanovg $ createFont "resources/Roboto-Bold.ttf"

    let render = do
                glClear $ GL_COLOR_BUFFER_BIT
                frame nanovg windowResolution $ do
                        withPath False $ do
                            -- Boxed "Aloha !"
                            fontFace robotoFont
                            fontSize 20
                            fillColor (Color 1 0 0 1)
                            void $ text 20 "Aloha !" 
                            (position, dims) <- textBounds 20 "Aloha !"
                            withPath False $ do
                                rect position dims
                                stroke


                            -- Boxed Aloha paragrpah
                            fontSize 10
                            fillColor (Color 1 0 1 1)
                            let text1 = "Aloha !fezfezf ezfez fezf ezfez"
                            textBox 
                                (V2 30 40) 
                                60
                                text1
                            (position, dims) <- textBoxBounds (V2 30 40) 60 text1
                            withPath False $ do
                                rect position dims
                                stroke


                            -- Centered paragraph
                            fillColor (Color 0 0 1 1)
                            textAlign $ Align CenterAlign Baseline
                            byteStringBox 
                                (V2 80 40) 
                                60
                                "Aloha !fezfezf ezfez fezf ezfez"

                            -- Paragraph with wide interlines
                            fillColor (Color 0 1 1 1)
                            textAlign $ Align LeftAlign Baseline
                            textLineHeight    1.25
                            textLetterSpacing 5
                            byteStringBox 
                                (V2 30 80) 
                                60
                                "Aloha !fezfezf ezfez fezf ezfez"

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
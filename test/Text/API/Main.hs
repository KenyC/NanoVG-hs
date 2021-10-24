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
--
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString    as BS

import Graphics.NanoVG
import Graphics.NanoVG.Color
import Graphics.NanoVG.Context
import Graphics.NanoVG.Text
import Graphics.NanoVG.Draw
import Graphics.NanoVG.Path
import Graphics.NanoVG.Transform
import Glew



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

    nanovg     <- nvgGL3Context [Debug]
    Just robotoFont <- withContext nanovg $ createFont "resources/Roboto-Bold.ttf"

    let render = do
                glClear $ GL_COLOR_BUFFER_BIT
                frame nanovg windowResolution $ do
                    -- Boxed "Aloha !" with 'h' on colored background
                    fontFace robotoFont
                    fontSize 20
                    fillColor (Color 1 0 0 1)
                    let label = "Aloha !"
                        pos   = 20
                    void $ text pos label 
                    void $ text pos "" 
                    (position, dims) <- textBounds pos label
                    withPath Open $ do
                        rect position dims
                        stroke

                    glyphs <- byteStringGlyphPos (Text.encodeUtf8 label) pos
                    FontMetrics ascending descending lineHeight <- fontMetrics
                    let GlyphPosition _ _ minX maxX = glyphs !! 3
                    withPath Open $ do
                        rect 
                            (V2 minX $ 20 - ascending)
                            (V2 (maxX - minX) lineHeight)
                        fillColor $ fromRGBA 0 0 0 64
                        fill



                    -- Boxed Aloha paragrpah
                    fontSize 10
                    fillColor (Color 1 0 1 1)
                    let text1 = "Aloha !fezfezf ezfez fezf ezfez"
                    textBox 
                        (V2 30 40) 
                        60
                        text1
                    (position, dims) <- textBoxBounds (V2 30 40) 60 text1
                    withPath Open $ do
                        rect position dims
                        stroke


                    -- Centered paragraph
                    fillColor (Color 0 0 1 1)
                    textAlign $ Align CenterAlign Baseline
                    byteStringBox 
                        (V2 80 40) 
                        60
                        "Aloha !fezfezf ezfez fezf ezfez"

                    -- Paragraph with wide interlines, underlined
                    fillColor (Color 0 1 1 1)
                    textAlign $ Align LeftAlign Baseline
                    textLineHeight    1.25
                    textLetterSpacing 5
                    byteStringBox 
                        (V2 30 80) 
                        60
                        "Aloha !fezfezf ezfez fezf ezfez"

                    -- Paragraph with underline
                    withNewState $ do
                        reset
                        textAlign $ Align LeftAlign Baseline
                        fillColor $ fromRGB 45 145 63
                        translate $ V2 90 90
                        let text  = "I am an alien. I am a legal alien."
                            width = 60
                        byteStringBox
                            0 width
                            text
                            
                        FontMetrics _ _ lineHeight <- fontMetrics
                        rows <- byteStringBreakLines text width
                        forM_ (zip [0..] rows) $ \(i, TextRow _ _ width _ _) -> do
                            withPath Open $ do
                                let y = i * lineHeight + 3 -- litte below baseline
                                moveTo $ V2 0     y
                                lineTo $ V2 width y
                                stroke

                    -- test null pointer ByteString's
                    void $ byteString 20 $ BS.drop 2 ""

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
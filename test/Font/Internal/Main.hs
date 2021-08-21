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
import Data.Bits
--
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.String
--
import Linear.V2

import Graphics.NanoVG
import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.Font
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

    nanovg <- nvgGL3Context [debug]
    fontId <- withForeignPtr (_getNVGContext nanovg) $ \ptr -> 
                withCString "normal"                     $ \name     ->
                withCString "resources/Roboto-Light.ttf" $ \filename ->
                    c_createFont ptr name filename

    fontIdBold <- withForeignPtr (_getNVGContext nanovg) $ \ptr -> 
        withCString "bold"                       $ \name     ->
        withCString "resources/Roboto-Bold.ttf"  $ \filename ->
            c_createFont ptr name filename
                    
    putStr "Font ID normal: "
    print fontId

    putStr "Font ID bold: "
    print fontIdBold

    -- defaultFontName <- peekCString =<< c_getFontName
    defaultFontName <- peekCString c_defaultFontName
    putStr "Default font name: "
    putStrLn defaultFontName

    let render = do
                glClear $ GL_COLOR_BUFFER_BIT
                glClear $ GL_DEPTH_BUFFER_BIT
                glClear $ GL_STENCIL_BUFFER_BIT
                frame nanovg windowResolution $ do
                        liftIO $ withForeignPtr (_getNVGContext nanovg) $ \ptr -> do
                            withCString "Aloha !" $ \text1 -> do
                                withCString "Aloha !dzagjz ezfez fezf ezfez" $ \text2 -> do
                                    c_beginPath ptr
                                    c_fontFaceId ptr fontId

                                    c_fontSize      ptr 120
                                    c_fillColor     ptr 1 0 1 1
                                    void $ c_text
                                                ptr
                                                100 100
                                                text1
                                                nullPtr

                                    (xMin, yMin, xMax, yMax) <- withArray [0, 0, 0, 0] $ \bounds -> do
                                        void $ c_textBounds 
                                                    ptr 
                                                    100 100
                                                    text1
                                                    nullPtr
                                                    bounds
                                        xMin:yMin:xMax:yMax:_ <- peekArray 4 bounds
                                        return (xMin, yMin, xMax, yMax)

                                    c_beginPath ptr
                                    c_rect ptr 
                                           xMin yMin
                                           (xMax - xMin) (yMax - yMin)
                                    c_stroke ptr


                                    c_fontSize  ptr 18
                                    c_fontBlur  ptr 5
                                    c_fillColor ptr 1 0 0 1

                                    void $ c_textBox
                                                ptr
                                                100 200
                                                150
                                                text2
                                                nullPtr

                                    (xMin, yMin, xMax, yMax) <- withArray [0, 0, 0, 0] $ \bounds -> do
                                        void $ c_textBoxBounds
                                                    ptr
                                                    100 200
                                                    150
                                                    text2
                                                    nullPtr
                                                    bounds
                                        xMin:yMin:xMax:yMax:_ <- peekArray 4 bounds
                                        return (xMin, yMin, xMax, yMax)

                                    c_beginPath ptr
                                    c_rect ptr 
                                           xMin yMin
                                           (xMax - xMin) (yMax - yMin)
                                    c_stroke ptr

                                    c_fontBlur  ptr 0
                                    c_textAlign ptr $ _align_right .|. _align_bottom
                                    c_fillColor ptr 0 1 0 1
                                    void $ c_textBox
                                                ptr
                                                300 200
                                                150
                                                text2
                                                nullPtr

                                    c_fontBlur  ptr 0
                                    c_textAlign ptr $ _align_left
                                    c_fillColor ptr 1 1 0 1
                                    c_textLetterSpacing ptr 5
                                    c_textLineHeight    ptr 2
                                    void $ c_textBox
                                                ptr
                                                100 300
                                                150
                                                text2
                                                nullPtr



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
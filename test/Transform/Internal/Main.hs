{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import SDL (($=))
import qualified SDL
--
import Graphics.GL
--
import Control.Monad
import Control.Monad.IO.Class
--
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
--
import Linear.V2
--
import Text.Printf

import Graphics.NanoVG
import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.Transform
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

    let square :: Ptr () -> IO ()
        square ptr = do
                    c_beginPath ptr
                    c_rect ptr 10 10 20 20
                    c_strokeColor ptr 1 0 0 1
                    c_stroke ptr


    let printTransform :: Ptr CFloat -> IO ()
        printTransform ptr = do
            list :: [Float] <- map realToFrac <$> peekArray 6 ptr
            printf "%f %f %f\n" (list !! 0) (list !! 2) (list !! 4) 
            printf "%f %f %f\n" (list !! 1) (list !! 3) (list !! 5)
            putStrLn "0 0 1"

    let render = do
                glClear $ GL_COLOR_BUFFER_BIT
                frame nanovg windowResolution $ do
                        liftIO $ withForeignPtr (_getNVGContext nanovg) $ \ptr -> do
                            square ptr

                            c_translate ptr 100 200
                            square ptr

                            c_rotate ptr (pi/4)
                            square ptr

                            c_resetTransform ptr
                            c_translate ptr 300 100
                            square ptr

                            c_skewX ptr 2
                            square ptr

                            c_resetTransform ptr
                            c_skewY ptr 2
                            square ptr

                            c_resetTransform ptr
                            c_translate ptr 50 50
                            c_scale     ptr 3 2
                            square ptr

                            c_resetTransform ptr
                            withArray [0 | _ <- [1 .. 6]] $ \ptrTf -> do
                                c_currentTransform ptr ptrTf
                                printTransform ptrTf

                            c_translate ptr 3 4
                            c_scale ptr 1 2
                            withArray [0 | _ <- [1 .. 6]] $ \ptrTf -> do
                                c_currentTransform ptr ptrTf
                                printTransform ptrTf


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
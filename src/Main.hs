{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Render
import WindowState
import Glew
import qualified FpsWidget as Fps


quitEvent :: SDL.Event -> Bool
quitEvent event = case SDL.eventPayload event of 
    SDL.KeyboardEvent keyboardEvent -> SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                                       SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    SDL.QuitEvent -> True
    _         -> False



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

    nvgContext <- nvgGL3Context [debug]

  
    glClearColor 0.3 0.3 0.3 1

    let state = WindowState {
        time = 0,
        mousePosition = 0,
        graph = Fps.emptyGraph
    }

    let appLoop currentState = do

            SDL.glSwapWindow $ window
            render nvgContext windowResolution currentState
            events                 <- SDL.pollEvents
            time                   <- SDL.ticks

            let newTime   = (fromIntegral time) / 1000 
                frameTime = newTime - WindowState.time currentState
                newGraph  = Fps.updateGraph (graph currentState) $ frameTime * 1000


            SDL.P mousePosition    <- SDL.getAbsoluteMouseLocation
            SDL.delay 60
            unless (any quitEvent events) $ appLoop $ currentState {
                  time          = newTime
                , mousePosition = fromIntegral <$> mousePosition
                , graph         = newGraph
            }

    appLoop state
    SDL.destroyWindow window
    SDL.quit



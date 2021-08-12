{-# LANGUAGE RecordWildCards #-}
module Render where

import Control.Monad
--
import Linear.V2
import Linear.Vector
--
import Graphics.GL
import Graphics.NanoVG
import Graphics.NanoVG.Context
import Graphics.NanoVG.Draw
import Graphics.NanoVG.Path
import Graphics.NanoVG.Paint
import Graphics.NanoVG.Color
import Graphics.NanoVG.Transform

import WindowState

render :: NVGContext 
       -> WindowResolution
       -> WindowState
       -> IO ()
render context windowResolution WindowState{..} = do
    glClear GL_COLOR_BUFFER_BIT
    frame context windowResolution $  do
        withPath False $ do
            rect (V2 0 0) (V2 50 50)
            fillColor $ Color 1 0 0 1
            fill     

        let V2 width height = _size windowResolution
        drawGraph   
            (V2 0     (height/2))
            (V2 width (height/2))
            time

drawGraph :: V2 Float 
          -> V2 Float
          -> Float
          -> VG ()
drawGraph position@(V2 x y) dims@(V2 width height) time = do
    let parameters = [ (1.2345, 0.33457, 0.44)
                     , (0.68363, 1.3, 1.55)
                     , (1.1642, 0.3345, 1.24)
                     , (0.56345, 1.63, 0.14)
                     , (1.6245, 0.254, 0.3)
                     , (0.345, 0.03, 0.6)       ]
    let dx = width / 5
    let abstractYValues = [ 0.5 * (1 + sin (time * a + c * cos (b * time)))  
                          | (a, b, c) <- parameters ]
    let coordinates = [V2 
                        (x + i * dx)
                        (y + height * y * 0.8)
                      | (i, y) <- zip [0..] abstractYValues]

    let curveLine = do   
                        moveTo $ head coordinates
                        forM_ (zip coordinates (tail coordinates)) $ \(pt1, pt2) -> 
                            bezierTo 
                                (pt1 + (0.5 * dx) *^ unit _x)
                                (pt2 - (0.5 * dx) *^ unit _x)
                                pt2
    bgGradient <- linearGradient 
            position 
            (position + height *^ unit _y)
            (Color 0 0.625 0.75 0)
            (Color 0 0.625 0.75 0.25)

    withPath False $ do
        curveLine

        lineTo $ position + dims
        lineTo $ position + height *^ unit _y

        fillPaint bgGradient
        fill

                
    translate $ 2 *^ unit _y
    withPath False $ do
        curveLine
        strokeColor $ Color 0 0 0 0.125
        stroke

    translate $ -2 *^ unit _y
    withPath False $ do
        curveLine
        strokeColor $ Color 0 0.625 0.75 1
        stroke

    forM_ coordinates $ \position -> do
        ptGrad <- radialGradient
                (position + 2 *^ unit _y)
                3 8
                (Color 0 0 0 0.125) (Color 0 0 0 0)

        withPath False $ do
            rect 
                (position - 10 + V2 0 2)
                (V2 20 20)
            fillPaint ptGrad
            fill
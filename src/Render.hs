{-# LANGUAGE RecordWildCards #-}
module Render where

import Control.Monad
--
import Linear.V2
import Linear.Vector
import Linear.Metric
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

        let V2 width height = _size windowResolution
        drawGraph   
            (V2 0     (height/2))
            (V2 width (height/2))
            time

        drawEyes
            (V2 (width - 250) 50)
            (V2 150 100)
            mousePosition
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


    ------------------- Background -----------------
    withPath False $ do
        curveLine

        lineTo $ position + dims
        lineTo $ position + height *^ unit _y

        fillPaint bgGradient
        fill

    ------------------- GRAPH LINE -----------------
    strokeWidth 3
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


    ------------------- GRAPH POINTS -----------------
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

            
    strokeWidth 1


drawEyes 
    :: V2 Float
    -> V2 Float
    -> V2 Float
    -> Float
    -> VG ()
drawEyes
    position@(V2 x y)
    dims@(V2 w h)
    mousePosition@(V2 mx my)
    time
    = do

        let eyeSize@(V2 ex ey) = V2 
                               (w * 0.23)
                               (h * 0.5)

            leftEye@(V2 lx ly)  = position + eyeSize
            rightEye@(V2 rx ry) = position + (V2 (w - ex) ey) 
            br = min ex ey * 0.5
                
        ------------------- EYEBALL -----------------

        bg <- linearGradient
                (V2 x (y + h * 0.5))
                (V2 (x + w * 0.1) (y + h))
                (Color 0 0 0 0.125)
                (Color 0 0 0 0.0625)

        withPath False $ do
            ellipse 
                (leftEye + (V2 3 16))
                ex ey
            ellipse 
                (rightEye + (V2 3 16))
                ex ey
            fillPaint bg
            fill
            
        bg <- linearGradient
                (position + (V2 0 0.25) * dims)
                (position + (V2 0.1 1)  * dims)
                (Color 0.86 0.86 0.86 1)
                (Color 0.5  0.5  0.5  1)

        withPath False $ do
            ellipse 
                (V2 lx ly)
                ex ey
            ellipse 
                (V2 rx ry)
                ex ey
            fillPaint bg
            fill
        
        ------------------- PUPILS -----------------

        let blink = 1 - (sin $ 0.5 * time) ** 20 * 0.8

        let unnormalizedDiff = (mousePosition - rightEye) / (10 * eyeSize)
            normalizedDiff
                | norm unnormalizedDiff > 1 = normalize unnormalizedDiff
                | otherwise                 = unnormalizedDiff
            closedEye = unnormalizedDiff * eyeSize * (V2 0.4 0.5)


        fillColor $ Color 0.0625 0.0625 0.0625 1.0
        withPath False $ do
            ellipse
                (rightEye + closedEye + (ey * 0.25 * (1 -blink)) *^ unit _y)
                br
                (br * blink)
            fill

        withPath False $ do
            ellipse
                (leftEye + closedEye + (ey * 0.25 * (1 -blink)) *^ unit _y)
                br
                (br * blink)
            fill
            -- diff
            --     | normalize > 1

        ------------------- GLOSS -----------------
        gloss <- radialGradient 
                    (leftEye - eyeSize * (V2 0.25 0.5))
                    (ex * 0.1)
                    (ex * 0.75)
                    (Color 1 1 1 0.5)
                    (Color 1 1 1 0)

        withPath False $ do
            ellipse leftEye ex ey
            fillPaint gloss
            fill

        gloss <- radialGradient 
                    (rightEye - eyeSize * (V2 0.25 0.5))
                    (ex * 0.1)
                    (ex * 0.75)
                    (Color 1 1 1 0.5)
                    (Color 1 1 1 0)


        withPath False $ do
            ellipse rightEye ex ey
            fillPaint gloss
            fill

        return ()

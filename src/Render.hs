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
--
import qualified FpsWidget as Fps

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

        drawColorwheel 
            (V2 (width - 300) (height -300))
            (V2 250 250)
            time

        Fps.drawGraph graph 0 font


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
    withPath Open $ do
        curveLine

        lineTo $ position + dims
        lineTo $ position + height *^ unit _y

        fillPaint bgGradient
        fill

    ------------------- GRAPH LINE -----------------
    strokeWidth 3
    translate $ 2 *^ unit _y
    withPath Open $ do
        curveLine
        strokeColor $ Color 0 0 0 0.125
        stroke

    translate $ -2 *^ unit _y
    withPath Open $ do
        curveLine
        strokeColor $ Color 0 0.625 0.75 1
        stroke


    ------------------- GRAPH POINTS -----------------
    forM_ coordinates $ \position -> do
        ptGrad <- radialGradient
                (position + 2 *^ unit _y)
                3 8
                (Color 0 0 0 0.125) (Color 0 0 0 0)

        withPath Open $ do
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

        withPath Open $ do
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

        withPath Open $ do
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
        withPath Open $ do
            ellipse
                (rightEye + closedEye + (ey * 0.25 * (1 -blink)) *^ unit _y)
                br
                (br * blink)
            fill

        withPath Open $ do
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

        withPath Open $ do
            ellipse leftEye ex ey
            fillPaint gloss
            fill

        gloss <- radialGradient 
                    (rightEye - eyeSize * (V2 0.25 0.5))
                    (ex * 0.1)
                    (ex * 0.75)
                    (Color 1 1 1 0.5)
                    (Color 1 1 1 0)


        withPath Open $ do
            ellipse rightEye ex ey
            fillPaint gloss
            fill

        return ()


drawColorwheel :: V2 Float
               -> V2 Float
               -> Float
               -> VG ()
drawColorwheel position dims time = do
    let hue    = sin $ 0.12 * time
        center = position + 0.5 * dims
        outerRadius = (minimum dims) * 0.5 - 5 :: Float
        innerRadius = outerRadius - 20
        halfPixel   = 0.5 / outerRadius

    save

    forM_ [0 .. 5] $ \i -> do
        let beginArc = i       / 6  * 2 * pi - halfPixel :: Float
            endArc   = (i + 1) / 6  * 2 * pi - halfPixel
        withPath Closed $ do
            arc
                center outerRadius
                beginArc endArc
                True
            arc
                center innerRadius
                endArc beginArc
                False

        let startPos = center + (innerRadius + outerRadius) / 2 *^ (V2 (cos beginArc) (sin beginArc))
            endPos   = center + (innerRadius + outerRadius) / 2 *^ (V2 (cos endArc)   (sin endArc))

        gradient <- linearGradient
                        startPos endPos
                        (fromHSLA (beginArc / (2 * pi)) 1 0.55 1)
                        (fromHSLA (endArc   / (2 * pi)) 1 0.55 1)
                        -- (Color 1 0 0 1)
                        -- (Color 0 1 0 1)
        fillPaint gradient
        fill

    withPath Open $ do
        circle center $ innerRadius - 0.5
        circle center $ outerRadius + 0.5
        strokeColor $ Color 0 0 0 0.25
        strokeWidth 1
        stroke

    save

    ------------------- MARKER -----------------
    translate center
    rotate $ hue * 2 * pi
    strokeWidth 2
    withPath Open $ do
        rect
            (V2 (innerRadius - 1)               (-3))
            (V2 (outerRadius - innerRadius + 2) 6   )
        strokeColor $ fromRGBA 255 255 255 192
        stroke

    gradient <- boxGradient 
                    (V2 (innerRadius - 3)               (-5))
                    (V2 (outerRadius - innerRadius + 6) 10  )
                    2 4
                    (fromRGBA 0 0 0 128)
                    (fromRGBA 0 0 0 0  )
    withPath Open $ do
        rect 
            (V2 (innerRadius - 2 - 10)               (- 4 - 10))
            (V2 (outerRadius - innerRadius + 4 + 20) (8 + 20))
        rect 
            (V2 (innerRadius - 2)               (-4))
            (V2 (outerRadius - innerRadius + 4) 8   )

        fillPaint gradient
        fill

    ------------------- COLOR TRIANGLE -----------------

    let sizeTriangle = innerRadius - 6
        pointA       = sizeTriangle *^ (angle $ 120/180 * pi)
        pointB       = sizeTriangle *^ (angle $ - 120/180 * pi)
        pointC       = V2 sizeTriangle 0

    withPath Closed $ do
        moveTo pointC 
        lineTo pointA
        lineTo pointB

    paint <- linearGradient 
                    pointC
                    pointA
                    (fromHSLA hue 1 0.5 1)
                    (Color 1 1 1 1)
    fillPaint paint
    fill


    paint <- linearGradient 
                    ((pointC + pointA) / 2)
                    pointB
                    (Color 0 0 0 0)
                    (Color 0 0 0 1)
    fillPaint paint
    fill

    strokeColor $ fromRGBA 0 0 0 64
    stroke

    ------------------- SELECT CIRCLE ON TRIANGLE -----------------

    let selectPos = pointA * (V2 0.3 0.4)
    strokeWidth 2
    withPath Open $ do
        circle selectPos 5
        strokeColor $ fromRGBA 255 255 255 192
        stroke

    paint <- radialGradient
                selectPos
                7 9
                (fromRGBA 0 0 0 64)
                (fromRGBA 0 0 0 0)
    withPath Open $ do
        rect
            (selectPos - 20)
            40
        circle selectPos 7
        fillPaint paint
        fill

    restore
    restore

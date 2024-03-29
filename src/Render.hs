{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render where

import Control.Monad
--
import Data.ByteString (ByteString)
import qualified Data.ByteString    as BS
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import Data.List (elemIndex)
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
import Graphics.NanoVG.Text
import Graphics.NanoVG.Scissor
import Graphics.NanoVG.Transform
--
import qualified FpsWidget as Fps

import WindowState
import Widgets
import Icons

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

        drawParagraph
            normalFont
            (V2 (width - 450) 50)
            (V2 150 100)
            mousePosition

        drawColorwheel 
            (V2 (width - 300) (height -300))
            (V2 250 250)
            time

        drawLines 
            (V2 120 (height - 50))
            (V2 600 50)
            time

        drawWindow "Widgets `n Stuff" boldFont (V2 50 50) (V2 300 400)

        let posSearchBox = V2 60 95
        drawSearchBox "Search" normalFont iconFont posSearchBox (V2 280 25)

        let posDropDown  = posSearchBox + V2 0 40
        drawDropDown "Effects" normalFont iconFont posDropDown (V2 280 28)

        let V2 _ popY     = posDropDown + V2 0 14
        let posLabelForm  = posDropDown + V2 0 45
        drawLabel "Login" normalFont posLabelForm (V2 200 20)

        let posEmail = posLabelForm + V2 0 25
        drawEditBox "Email" normalFont posEmail (V2 280 28)

        let posPassword = posEmail + V2 0 35
        drawEditBox "Password" normalFont posPassword (V2 280 28)

        let posCheckBox = posPassword + V2 0 38
        drawCheckBox "Remember me" normalFont iconFont posCheckBox (V2 140 28)
        drawButton 
            (Just iconLogin) "Sign in" 
            normalFont iconFont 
            (posCheckBox + V2 138 0) (V2 140 28) 
            (fromRGBA 0 96 128 255)

        let posDiameter = posCheckBox + V2 0 45
        drawLabel "Diameter" normalFont posDiameter (V2 280 20)

        let posNum = posDiameter + V2 0 25
        drawEditBoxNum 
            "123.00" "px" 
            normalFont
            (posNum + V2 180 0) (V2 100 28)

        drawSlider 0.4 posNum (V2 170 28)

        let posButtons = posNum + V2 0 55
        drawButton 
            (Just iconTrash) "Delete" 
            normalFont iconFont
            posButtons
            (V2 160 28)
            (fromRGBA 128 16 8 255)
        drawButton 
            Nothing "Cancel" 
            normalFont iconFont
            (posButtons + V2 170 0)
            (V2 110 28)
            (fromRGBA 0 0 0 0)

        drawThumbnails
            (V2 365 (popY - 30))
            (V2 160 300)
            images
            time


        Fps.drawGraph graph 0 normalFont


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

        pathWinding Hole
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



drawLines :: V2 Float 
          -> V2 Float
          -> Float
          -> VG ()
drawLines position dims@(V2 width height) time = do
    let pad    = 5
    let widthLine = width / 9 - 2 * pad
    let start:points = [ V2 (- widthLine * 0.25) 0 + widthLine * 0.5 *^ angle (0.3 * time) 
                       , V2 (- widthLine * 0.25) 0
                       , V2 (widthLine * 0.25) 0
                       , V2 (widthLine * 0.25) 0 + widthLine * 0.5 *^ angle (- 0.3 * time) ]
    
    let joins = [Miter, RoundJoin, Bevel]
    let caps  = [Butt,  RoundCap,  Square]

    let enumerate = zip [0..]

    withNewState $ 
        forM_ (enumerate joins) $ \(i :: Float, join) ->
            forM_ (enumerate caps) $ \(j :: Float, cap) -> 
                withNewState $ do
                    translate $ position + widthLine * 0.5 *^ V2 1 (-1) + pad *^ 1  + (3 * i + j) / 9 *^ V2 width 0

                    lineCap  cap
                    lineJoin join
                    strokeWidth $ widthLine * 0.3
                    strokeColor $ fromRGBA 0 0 0 160

                    withPath Open $ do
                        moveTo start
                        mapM_ lineTo points
                        stroke

                    lineCap  Butt
                    lineJoin Bevel
                    strokeWidth 1
                    strokeColor $ fromRGBA 0 192 255 255

                    withPath Open $ do
                        moveTo start
                        mapM_ lineTo points
                        stroke


drawWindow :: ByteString
           -> Font
           -> V2 Float
           -> V2 Float
           -> VG ()
drawWindow title font pos dims@(V2 width height) = do
    let cornerRadius = 3
    withNewState $ do
        translate pos

        -- window frame
        withPath Open $ do
            roundedRect 0 dims cornerRadius
            fillColor $ fromRGBA 28 30 34 192
            fill

        -- drop shadow
        shadowPaint <- boxGradient 
                (V2 0 2) dims 
                (cornerRadius * 2)
                10
                (fromRGBA 0 0 0 128)
                (fromRGBA 0 0 0 0)
        withPath Open $ do
            rect (-10) (dims + V2 20 30)
            roundedRect 0 dims cornerRadius
            pathWinding Hole
            fillPaint shadowPaint
            fill

        -- header
        headerPaint <- linearGradient 0 (V2 0 15)
                            (fromRGBA 255 255 255 8 )
                            (fromRGBA 0   0   0   16)
        withPath Open $ do
            roundedRect 
                1
                (V2 (width - 2) 30)
                (cornerRadius - 1)
            fillPaint headerPaint
            fill

        withPath Open $ do
            let start = 0.5 + V2 0 30
            moveTo start 
            lineTo $ start + V2 (width - 1) 0
            strokeColor $ fromRGBA 0 0 0 32
            stroke
            
        -- header label
        fontSize 15
        fontFace font
        textAlign $ Align CenterAlign Middle

        fontBlur 2
        fillColor $ fromRGBA 0 0 0 128
        byteString 
            (V2 (width / 2) (16 + 1))
            title

        fontBlur 0
        fillColor $ fromRGBA 220 220 220 160
        byteString 
            (V2 (width / 2) (16 + 1))
            title

        return ()

drawParagraph :: Font
              -> V2 Float
              -> V2 Float
              -> V2 Float
              -> VG ()
drawParagraph font position dims@(V2 width _) mousePosition = do
    let paragraph :: ByteString
        paragraph = Text.encodeUtf8 "This is longer chunk of text.\n  \n  Would have used lorem ipsum but she    was busy jumping over the lazy dog with the fox and all the men who came to the aid of the party.🎉"

    let relMousePos@(V2 relMousePosX relMousePosY) = mousePosition - position
    let slice start end = (BS.take (end - start)) . (BS.drop start)
    -- let slice start end = (BS.take (end - start - 1)) . (BS.drop 1000)


    withNewState $ do
        fontFace font
        FontMetrics _ _ lineHeight <- fontMetrics

        textRows <- byteStringBreakLines paragraph width

        translate position
        forM_ (zip [0..] textRows) $ \(i, TextRow{..}) -> do
            let hit = all (>=0) (relMousePos - (V2 0 (i * lineHeight))) && 
                      all (> 0) ((V2 width ((i+1) * lineHeight) - relMousePos))

            let row    = slice _startIndex _endIndex paragraph
            let rowPos = V2 0 (i * lineHeight)
            let lineWidth = _maxX - _minX


            withPath Open $ do
                rect (V2 _minX (i * lineHeight))
                     (V2 lineWidth lineHeight)
            fillColor $ fromRGBA 255 255 255 $ if hit then 64 
                                                      else 16
            fill

            fontSize 15
            textAlign $ Align LeftAlign Top
            fillColor  $ fromRGB 255 255 255
            byteString rowPos row

            when hit $ do

                ------------------- Print caret -----------------
                glyphPos <- byteStringGlyphPos row rowPos
                let positions = (map _logicalX glyphPos) ++ [lineWidth]
                    midPoints = (0:) $ flip map (zip positions $ tail positions) $ \(x, y) -> 0.3 * x + 0.7 * y 
                    caretX = case elemIndex True $ map (> relMousePosX) midPoints of
                                Just i  -> positions !! i
                                Nothing -> 0

                withPath Open $ do
                    rect
                        (rowPos + V2 caretX 0)
                        (V2 1 lineHeight)
                
                fillColor $ fromRGB 255 192 0
                fill

                ------------------- Print Gutter -----------------
                let gutterPos  = rowPos + (V2 (-10) (lineHeight / 2))
                let textLineNo = Text.pack $ show $ floor i
                fontSize 12
                textAlign $ Align RightAlign Middle
                (textPos, textDims) <- textBounds
                                         gutterPos
                                         textLineNo

                withPath Open $ do
                    roundedRect
                        (textPos  - V2 4 2)
                        (textDims + V2 8 4)
                        3
                fillColor $ fromRGB 255 192 0
                fill

                fillColor $ fromRGB 32 32 32
                void $ text gutterPos textLineNo

        let tooltipText = "Hover your mouse over the text to see calculated caret position."

        fontSize 11
        textAlign $ Align LeftAlign Top
        textLineHeight 1.2



        let tooltipPos_  = V2 0 ((fromIntegral $ length textRows) * lineHeight)
        (tooltipPos, tooltipDims) <- byteStringBoxBounds tooltipPos_ 150 tooltipText

        withPath Open $ do
            roundedRect 
                (tooltipPos  - 2)
                (tooltipDims + 4)
                3
            let V2 midX _   = (tooltipPos + tooltipDims) / 2
                V2 _ yPos   = tooltipPos
            moveTo $ V2 midX (yPos - 10)
            lineTo $ V2 (midX - 7) (yPos + 1)
            lineTo $ V2 (midX + 7) (yPos + 1)


        fillColor $ fromRGB 220 220 220
        fill

        fillColor $ fromRGB 0 0 0
        byteStringBox tooltipPos_ 150 tooltipText

drawThumbnails :: V2 Float
               -> V2 Float
               -> [Image]
               -> Float
               -> VG ()
drawThumbnails pos dims@(V2 width height) images time = do
        let cornerRadius = 3
            arrY         = 30.5

        withNewState $ do
            translate pos

            -- Shadow
            shadowPaint <- boxGradient 
                    (V2 0 4) dims
                    (cornerRadius * 2) 20
                    (fromRGBA 0 0 0 128)
                    (fromRGBA 0 0 0 0)
            withPath Open $ do
                rect 
                    (-10)
                    (dims + V2 20 30)
                roundedRect 0 dims cornerRadius
                pathWinding Hole
            fillPaint shadowPaint
            fill

            -- Window
            withPath Open $ do
                roundedRect 0 dims cornerRadius
                moveTo $ V2 (-10) arrY 
                lineTo $ V2 1     (arrY-11)
                lineTo $ V2 1     (arrY+11)
            fillColor $ fromRGB 200 200 200
            fill

            -- images
            let thumbnailSize = 60 :: Float
            let stackHeight  = ((fromIntegral $ length images) / 2) * (thumbnailSize + 10) + 10
            let scrollH      = (height / stackHeight) * (height - 8)
            let scrollLevel  = (1 + (cos $ time * 0.5)) * 0.5
            let spinnerLevel = (1 - (cos $ time * 0.2)) * 0.5

            withNewState $ do
                scissor 0 dims
                translate $ V2 0 $ - (stackHeight - height) * scrollLevel
                forM_ (zip [0 ..] images) $ \(i, image) -> do
                    imageDims <- imageSize image

                    let V2 width height = fromIntegral <$> imageDims
                        approxPlace = 10 + (thumbnailSize + 10) *^ (fromIntegral <$> V2 (i `rem` 2) (i `quot` 2))
                        aspectRatio = width / height
                        imageDisplaySize
                            | width > height = V2 thumbnailSize (thumbnailSize / aspectRatio)
                            | otherwise      = V2 (aspectRatio * thumbnailSize) thumbnailSize

                        positionImage = approxPlace - 0.5 *^ (imageDisplaySize - thumbnailSize *^ 1)


                    -- spinner
                    let dVelocity      = 1 / (fromIntegral $ length images - 1)
                    let velocity       = (fromIntegral i) * dVelocity

                    let loadLevel      = min 1 $ max 0 $ (spinnerLevel - velocity) / dVelocity

                    when (loadLevel < 1) $ do
                        drawSpinner 
                            (positionImage + 0.5 *^ imageDisplaySize)
                            (thumbnailSize * 0.25) 
                            time


                    imagePaint <- imagePattern positionImage imageDisplaySize 0 loadLevel image
                    -- imagePaint <- imagePattern 0 200 0 1 image
                    withPath Open $ do
                        roundedRect 
                            positionImage 
                            imageDisplaySize
                            5
                            -- (V2 thumbnailSize thumbnailSize)
                    fillPaint imagePaint
                    fill

                    shadowPaint <- boxGradient 
                                        (positionImage - V2 1 0)
                                        (imageDisplaySize + 2)
                                        5 3
                                        (fromRGBA 0 0 0 128)
                                        (fromRGBA 0 0 0 0)
                    withPath Open $ do
                        rect
                            (positionImage - 5)
                            (imageDisplaySize + 10)
                        roundedRect positionImage imageDisplaySize 6
                        pathWinding Hole
                    fillPaint shadowPaint
                    fill

                    withPath Open $ do
                        roundedRect 
                            (positionImage + 0.5)
                            (imageDisplaySize - 1)
                            (4 - 0.5)
                    strokeWidth 1
                    strokeColor $ fromRGBA 255 255 255 192
                    stroke
                    return ()

            -- Hide fades : not sure what this is supposed to do visually
            -- fadePaint <- linearGradient 
            --                     0 (V2 0 6)
            --                     (fromRGBA 255 0 0 255)
            --                     (fromRGBA 200 200 200 0)
            --                     -- (fromRGBA 200 200 200 255)
            -- withPath Open $ 
            --     rect 
            --         (V2 4 0)
            --         (V2 (width - 8) 6)
            -- fillPaint fadePaint
            -- fill



            -- scroll bars
            shadowPaint <- boxGradient
                                (V2 (width - 12 + 1) (4 + 1))
                                (V2 8 (height - 8))
                                3 4
                                (fromRGBA 0 0 0 32)
                                (fromRGBA 0 0 0 92)
            withPath Open $
                roundedRect
                    (V2 (width - 12) 4)
                    (V2 8 (height - 8))
                    3
            fillPaint shadowPaint
            fill


            shadowPaint <- boxGradient
                                (V2 (width - 12) (4 + (height - 8 - scrollH) * scrollLevel - 1))
                                (V2 8 scrollH)
                                3 4
                                (fromRGB 220 220 220)
                                (fromRGB 128 128 128)
            withPath Open $
                roundedRect
                    (V2 (width - 12 + 1) (4 + 1 + (height - 8 - scrollH) * scrollLevel))
                    (V2 (8-2) (scrollH - 2))
                    2
            fillPaint shadowPaint
            fill

drawSpinner :: V2 Float -- middle position
            -> Float    -- radius
            -> Float    -- time
            -> VG ()
drawSpinner midPos outerRadius time = withNewState $ do
    let innerRadius = 0.75 * outerRadius
        angleInit   = 0  + time * 6
        angleEnd    = pi + time * 6

    withPath Closed $ do
        arc midPos outerRadius angleInit angleEnd  True
        arc midPos innerRadius angleEnd  angleInit False

    let posInit = midPos + angle angleInit ^* ((innerRadius + outerRadius) / 2)
    let posEnd  = midPos + angle angleEnd  ^* ((innerRadius + outerRadius) / 2)

    paint <- linearGradient 
                    posInit posEnd
                    (fromRGBA 0 0 0 0)
                    (fromRGBA 0 0 0 128)
    fillPaint paint
    fill

{-# LANGUAGE OverloadedStrings #-}
module Widgets where

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
import Graphics.NanoVG.Text
import Graphics.NanoVG.Transform
--
import Data.ByteString (ByteString)
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as Text

import Icons

drawSearchBox :: ByteString
              -> Font
              -> Font
              -> V2 Float
              -> V2 Float
              -> VG ()
drawSearchBox label normalFont iconFont pos dims@(V2 width height) = do
    let cornerRadius = height / 2 - 1

    bg <- boxGradient
                (pos + V2 0 1.5)
                dims
                (height/2) 5
                (fromRGBA 0 0 0 16)
                (fromRGBA 0 0 0 92)
    withPath Open $ do
        roundedRect pos dims cornerRadius
        fillPaint bg
        fill

    fontSize $ height * 1.3
    fontFace iconFont
    fillColor $ fromRGBA 255 255 255 64
    textAlign $ Align CenterAlign Middle
    byteString
        (pos + height * 0.55 *^ 1)
        iconSearch     

    fontSize 17
    fontFace normalFont
    fillColor $ fromRGBA 255 255 255 32 
    textAlign $ Align LeftAlign Middle
    byteString
        (pos + height *^ V2 1.05 0.5)
        label


    fontSize $ height * 1.3
    fontFace iconFont
    fillColor $ fromRGBA 255 255 255 32
    textAlign $ Align CenterAlign Middle
    byteString
        (pos + V2 width 0 + height * 0.55 *^ V2 (-1) 1)
        iconCircledCross    



    return ()


drawDropDown :: ByteString
             -> Font
             -> Font
             -> V2 Float
             -> V2 Float
             -> VG ()
drawDropDown 
    label 
    normalFont iconFont 
    pos dims@(V2 width height) = do
        let cornerRadius = 4
        bg <- linearGradient
                pos (pos + V2 0 height)
                (fromRGBA 0   0   0   16)
                (fromRGBA 255 255 255 16)

        withPath Open $ do
            roundedRect (pos + 1) (dims - 2) (cornerRadius - 1)
            fillPaint bg
            fill

        withPath Open $ do
            roundedRect (pos + 0.5) (dims - 1) (cornerRadius - 0.5)
            strokeColor $ fromRGBA 0 0 0 48
            stroke

        fontSize 17
        fontFace normalFont
        fillColor $ fromRGBA 255 255 255 160
        textAlign $ Align LeftAlign Middle
        byteString
            (pos + height *^ V2 0.3 0.5)
            label

        fontSize $ height * 1.3
        fontFace iconFont
        fillColor $ fromRGBA 255 255 255 64
        textAlign $ Align CenterAlign Middle
        byteString
            (pos + V2 width 0 + height * 0.5 *^ V2 (-1) 1)
            iconChevronRight

        return ()


drawLabel :: ByteString
          -> Font
          -> V2 Float
          -> V2 Float
          -> VG ()
drawLabel 
    label 
    normalFont 
    pos dims@(V2 width height) = do
        fontSize 15
        fontFace normalFont
        fillColor $ fromRGBA 255 255 255 128
        textAlign $ Align LeftAlign Middle
        byteString
            (pos + V2 0 (height * 0.5))
            label
        return ()

drawEditBox :: ByteString
            -> Font
            -> V2 Float
            -> V2 Float
            -> VG ()
drawEditBox 
    label 
    font
    pos dims@(V2 width height) = do
        drawEditBoxBase pos dims

        fontSize 17
        fontFace font
        fillColor $ fromRGBA 255 255 255 64
        textAlign $ Align LeftAlign Middle
        byteString
            (pos + height *^ V2 0.3 0.5)
            label
        return ()

drawEditBoxNum :: ByteString
               -> ByteString
               -> Font
               -> V2 Float
               -> V2 Float
               -> VG ()
drawEditBoxNum 
    measure unit
    font
    pos dims@(V2 width height) = do
        drawEditBoxBase pos dims

        (_, V2 unitWidth _) <- byteStringBounds 0 unit

        fontSize 15
        fontFace font
        fillColor $ fromRGBA 255 255 255 64
        textAlign $ Align RightAlign Middle
        byteString
            (pos + V2 width 0 + height *^ V2 (-0.3) 0.5)
            unit

        fontSize 17
        fontFace font
        fillColor $ fromRGBA 255 255 255 128
        textAlign $ Align RightAlign Middle
        byteString
            (pos + V2 (width - unitWidth) 0 + height *^ V2 (- 0.5) 0.5)
            measure
        return ()


drawEditBoxBase :: V2 Float
                -> V2 Float
                -> VG ()
drawEditBoxBase pos dims = do
    bg <- boxGradient (pos + V2 1 (1+1.5)) (dims - 2) 3 4 
            (fromRGBA 255 255 255 32)
            (fromRGBA 32  32  32  32)

    withPath Open $ do
        roundedRect 
            (pos  + 1)
            (dims - 2)
            (4 - 1)
        fillPaint bg
        fill

    withPath Open $ do
        roundedRect 
            (pos  + 0.5)
            (dims - 1)
            (4 - 0.5)
        strokeColor $ fromRGBA 0 0 0 48
        stroke


drawCheckBox :: ByteString
             -> Font
             -> Font
             -> V2 Float
             -> V2 Float
             -> VG ()
drawCheckBox 
    label 
    normalFont iconFont 
    pos dims@(V2 width height) = do
        fontSize 15
        fontFace normalFont
        fillColor $ fromRGBA 255 255 255 160

        textAlign $ Align LeftAlign Middle
        byteString
            (pos + V2 28 (height * 0.5))
            label

        bg <- boxGradient 
                (pos + V2 1 (height * 0.5 - 9 + 1))
                18
                3 3
                (fromRGBA 0 0 0 32)
                (fromRGBA 0 0 0 9)
        withPath Open $ do
            roundedRect 
                (pos + V2 1 (height * 0.5 - 9))
                18
                3
            fillPaint bg
            fill

        fontSize 32
        fontFace iconFont
        fillColor $ fromRGBA 255 255 255 128
        textAlign $ Align CenterAlign Middle
        byteString
            (pos + V2 (9 + 2) (height * 0.5))
            iconCheck
        return ()


drawButton :: Maybe ByteString
           -> ByteString
           -> Font
           -> Font
           -> V2 Float
           -> V2 Float
           -> Color
           -> VG ()
drawButton 
    maybeIcon label 
    font 
    iconFont
    pos dims@(V2 width height) 
    color = do
        let cornerRadius = 4
            isBlack = case color of
                        Color 0 0 0 _ -> True
                        _             -> False
            transparency = if isBlack then 16 else 32

        bg <- linearGradient 
                    pos (pos + V2 0 height)
                    (fromRGBA 255 255 255 transparency)
                    (fromRGBA 0   0   0   transparency)

        -- Button background
        withPath Open $ do
            roundedRect (pos + 1) (dims - 2) (cornerRadius - 1)
            when (not isBlack) $ do
                fillColor color
                fill
            fillPaint bg
            fill

        -- Button border
        withPath Open $ do
            roundedRect (pos + 0.5) (dims - 1) (cornerRadius - 0.5)
            strokeColor $ fromRGBA 0 0 0 48
            stroke

        -- Button label
        fontSize 17
        fontFace font
        (_, V2 textWidth _) <- byteStringBounds 0 label

        -- If there is an icon, compute its width
        iconWidth <- case maybeIcon of
            Nothing   -> return 0
            Just icon -> do
                fontSize (height * 1.3)
                fontFace iconFont
                (_, V2 width _) <- byteStringBounds 0 icon 
                return $ width + height * 0.15

        -- If there is an icon, draw it
        case maybeIcon of 
            Nothing   -> return ()
            Just icon -> do
                fontSize (height * 1.3)
                fontFace iconFont
                fillColor $ fromRGBA 255 255 255 96
                textAlign $ Align LeftAlign Middle
                void $ byteString
                    (pos + V2 (width * 0.5 - textWidth * 0.5 - iconWidth * 0.75) 
                              (height * 0.5))
                    icon




        -- Draw label with a bit of shadow
        fontSize 17
        fontFace font
        textAlign $ Align LeftAlign Middle
        fillColor $ fromRGBA 0 0 0 96
        let textPos = pos + V2 (width * 0.5 - textWidth * 0.5 + iconWidth * 0.25) (height * 0.5 -1)
        byteString
            textPos
            label

        fillColor $ fromRGBA 255 255 255 160
        byteString
            (textPos + V2 0 1)
            label


        return ()

drawSlider :: Float
           -> V2 Float
           -> V2 Float
           -> VG ()
drawSlider value pos dims@(V2 width height) = do 
    withNewState $ do
        translate pos
        let midY       = height / 2
            knobRadius = height / 4

        -- slot
        bg <- boxGradient 
                    (V2 0     (midY - 2 + 1))
                    (V2 width 4)
                    2 2
                    (fromRGBA 0 0 0 32)
                    (fromRGBA 0 0 0 128)
        withPath Open $ do
            roundedRect 
                (V2 0 (midY - 2))
                (V2 width 4)
                2
            fillPaint bg
            fill

        -- knob shadow
        let positionOnSlider = value * width
        bg <- radialGradient 
                (V2 positionOnSlider (midY + 1))
                (knobRadius - 3) (knobRadius + 3)
                (fromRGBA 0 0 0 64)
                (fromRGBA 0 0 0 0 )

        withPath Open $ do
            rect 
                (V2 positionOnSlider 0 - (knobRadius + 5) *^ 1)
                ((knobRadius * 2 + 5) *^ 1 + V2 5 3)
            circle (V2 positionOnSlider midY) knobRadius
            pathWinding Hole
            fillPaint bg
            fill

        -- knob
        bg <- linearGradient 
                (V2 0 (midY - knobRadius))
                (V2 0 (midY + knobRadius))
                (fromRGBA 255 255 255 16)
                (fromRGBA 0   0   0   16)
        withPath Open $ do
            circle (V2 positionOnSlider midY) $ knobRadius - 1
            fillColor $ fromRGBA 40 43 48 255
            fill
            fillPaint bg
            fill

        withPath Open $ do
            circle (V2 positionOnSlider midY) $ knobRadius - 0.5
            strokeColor $ fromRGBA 0 0 0 92
            stroke






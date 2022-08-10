module Graphics.NanoVG(
    frame,

    -- RE-EXPORTS
    -- Color.hs
    Color(..),
    fromRGB,
    fromRGBA,

    -- Context.hs
    NVGContext,
    WindowResolution(..),
    VG(),
    InitFlag(..),
    nvgGL3Context,
    withContext,
    save,
    restore,
    reset,
    withNewState,

    -- Draw.hs
    LineCapStyle(..),
    lineCap,
    LineJoinStyle(..),
    lineJoin,
    stroke,
    strokeColor,
    fill,
    fillColor,

    -- Image.hs
    ImageFlag(..),
    Image,
    withImage,
    imageSize,
    imagePattern,

    -- Paint.hs
    Paint,
    strokePaint,
    fillPaint,
    linearGradient,
    boxGradient,
    radialGradient,

    -- Path.hs
    PathType(..),
    withPath,
    moveTo,
    lineTo,
    bezierTo,
    arcTo,
    quadTo,
    rect,
    roundedRect,
    circle,
    arc,
    ellipse,

    -- Text.hs
    Font(..),
    createFont,
    fontBlur,
    fontFace,
    fontSize,
    HAlign(..),
    VAlign(..),
    Align(..),
    textAlign,
    textLetterSpacing,
    textLineHeight,
    text,
    textBox,
    byteString,
    byteStringBox,
    textBounds,
    textBoxBounds,
    byteStringBounds,
    byteStringBoxBounds,

    -- Transform.hs
    putTransform,
    getTransform,
    resetTransform,
    translate,
    rotate,
    scale
) where

import Foreign.ForeignPtr
--
import Linear.V2

import Graphics.NanoVG.Color
import Graphics.NanoVG.Context
import Graphics.NanoVG.Draw
import Graphics.NanoVG.Text
import Graphics.NanoVG.Image
import Graphics.NanoVG.Internal
import Graphics.NanoVG.Paint
import Graphics.NanoVG.Path
import Graphics.NanoVG.Transform

-- | Creates a drawing frame, i.e. a list of drawing instructions. The frame will be drawn on next screen refresh.
frame :: NVGContext       -- ^ NanoVG context
      -> WindowResolution -- ^ Window size and dpi
      -> VG ()            -- ^ Drawing instructions
      -> IO ()
frame 
    context@(NVGContext foreignPtr) 
    (WindowResolution (V2 width height) dpi) 
    cont = do 
                withForeignPtr foreignPtr $ \ptr -> do
                    c_beginFrame 
                        ptr 
                        (realToFrac width) 
                        (realToFrac height) 
                        (realToFrac dpi)
                    withContext context cont
                    c_endFrame ptr
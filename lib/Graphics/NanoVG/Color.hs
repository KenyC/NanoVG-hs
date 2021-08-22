{-|
Module      : Graphics.NanoVG.Color
Description : Color data types and utilities.
Copyright   : (c) Keny C, 2021
License     : MIT
Stability   : experimental

Contains the color data type 'Color' used by functions such as 'fillColor' and 'strokeColor'.
-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.NanoVG.Color where


-- | RGBA representation of color
data Color = Color {
    _red   :: Float, -- ^ red value, from 0 to 1 
    _green :: Float, -- ^ green value, from 0 to 1 
    _blue  :: Float, -- ^ blue value, from 0 to 1 
    _alpha :: Float  -- ^ alpha value, from 0 to 1
} deriving (Eq, Show)


-- | Create Color from RGBA values in range [0 .. 255] 
fromRGBA 
    :: Float
    -> Float
    -> Float
    -> Float
    -> Color
fromRGBA red green blue alpha = Color (red / 255) (green / 255) (blue / 255) (alpha / 255)

-- | Create Color from RGB values in range [0 .. 255]. Alpha component is set to maximum value.
fromRGB
    :: Float
    -> Float
    -> Float
    -> Color
fromRGB red green blue = Color (red / 255) (green / 255) (blue / 255) 1

lerpRGBA :: Color
         -> Color
         -> Float
         -> Color
lerpRGBA 
    (Color red1 blue1 green1 alpha1)
    (Color red2 blue2 green2 alpha2)
    factor
    = Color 
        (lerp red1    red2    factor)
        (lerp green1  green2  factor)
        (lerp blue1   blue2   factor)
        (lerp alpha1  alpha2  factor)
        where lerp a b t = t * a + (1 - t) * b

fromHSLA 
    :: Float
    -> Float
    -> Float
    -> Float
    -> Color
fromHSLA hue' saturation' luminance' alpha' =
    Color 
        (clamp $ mapTo $ hue + 1/3)
        (clamp $ mapTo $ hue      )
        (clamp $ mapTo $ hue - 1/3)
        alpha
    where clamp = (max 0) . (min 1)
          sextant val
            | val < 0    = sextant $ val + 1
            | val > 1    = sextant $ val - 1
            | val < 1/6  = 6 * val
            | val < 3/6  = 1
            | val < 4/6  = 4 - 6 * val
            | otherwise  = 0
          hue        = clamp hue'
          saturation = clamp saturation'
          luminance  = clamp luminance'
          alpha      = clamp alpha'
          maxRGB 
                | luminance <= 0.5 = luminance * (1 + saturation) 
                | otherwise        = luminance + saturation - luminance * saturation
          minRGB = 2 * luminance - maxRGB             
          mapTo x = minRGB + (sextant x) * (maxRGB - minRGB)

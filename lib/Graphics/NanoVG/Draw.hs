{-|
Module      : Graphics.NanoVG.Draw
Description : Rendering and rendering settings.
Copyright   : (c) Keny C, 2021
License     : MIT
Stability   : experimental

To draw something, you need to give NanoVG a path (i.e. a list of lines or strokes) and then a method for rendering these segments.
This module defines the functions for rendering a path ('stroke' or 'fill') and for tweaking rendering settings ('strokeColor', 'fillColor', 'strokeWidth', etc.)

For functions to create the path, see 'Graphics.NanoVG.Path'. 
-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.NanoVG.Draw (
      strokeColor
    , strokeWidth
    , stroke
    , fillColor
    , fill
    , CompositeOp(..)
    , globalCompositeOperation
    , LineCapStyle(..)
    , LineJoinStyle(..)
    , lineJoin
    , lineCap
    , miterLimit
    , globalAlpha
) where


import Graphics.NanoVG.Context
import Graphics.NanoVG.Color
import Graphics.NanoVG.Internal.Draw
import Graphics.NanoVG.Internal.Flag



-- | Data type for compositing operations. Documentation for the compisiting operation shamelessly stolen from [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Compositing) (cf here for pictures).
data CompositeOp =
      SourceOver         -- ^ This is the default setting and draws new shapes on top of the existing canvas content.                   
    | SourceIn           -- ^ The new shape is drawn only where both the new shape and the destination canvas overlap. Everything else is made transparent.         
    | SourceOut          -- ^ The new shape is drawn where it doesn't overlap the existing canvas content.          
    | SourceAtop         -- ^ The new shape is only drawn where it overlaps the existing canvas content.           
    | DestinationOver    -- ^ New shapes are drawn behind the existing canvas content.                
    | DestinationIn      -- ^ The existing canvas content is kept where both the new shape and existing canvas content overlap. Everything else is made transparent.              
    | DestinationOut     -- ^ The existing content is kept where it doesn't overlap the new shape.               
    | DestinationAtop    -- ^ The existing canvas is only kept where it overlaps the new shape. The new shape is drawn behind the canvas content.                
    | Lighter            -- ^ Where both shapes overlap the color is determined by adding color values.        
    | Copy               -- ^ Only the new shape is shown.     
    | Xor                -- ^ Shapes are made transparent where both overlap and drawn normal everywhere else.    
    | CustomOp         Int Int
    | CustomOpSeparate Int Int Int Int
    deriving (Eq, Show)

instance Flag CompositeOp where
    toCInt SourceOver      = _source_over
    toCInt SourceIn        = _source_in
    toCInt SourceOut       = _source_out
    toCInt SourceAtop      = _source_atop
    toCInt DestinationOver = _destination_over
    toCInt DestinationIn   = _destination_in
    toCInt DestinationOut  = _destination_out
    toCInt DestinationAtop = _destination_atop
    toCInt Lighter         = _lighter
    toCInt Copy            = _copy
    toCInt Xor             = _xor
    toCInt _               = error "Custom operations do not have CInt representations."



-- | Sets the composite operation. 
--   New shapes are composed over previously drawn shapes by the given operation.
globalCompositeOperation :: CompositeOp -> VG ()
globalCompositeOperation (CustomOp src dest) = 
    applyContext $ \ptr -> c_globalCompositeBlendFunc ptr
                                (fromIntegral src)
                                (fromIntegral dest)
globalCompositeOperation (CustomOpSeparate srcRGB destRGB srcA destA) =
    applyContext $ \ptr -> c_globalCompositeBlendFuncSeparate ptr
                                (fromIntegral srcRGB)
                                (fromIntegral destRGB)
                                (fromIntegral srcA)
                                (fromIntegral destA)
globalCompositeOperation op = applyContext $ \ptr -> 
                                    c_globalCompositeOperation 
                                        ptr 
                                        (toCInt op)


data LineCapStyle = Butt     -- ^ no cap ; line stops right at the extremity of the segment
                  | RoundCap -- ^ round cap
                  | Square   -- ^ square cap ; visually like 'Butt', but line continues beyond the extremity
                  deriving (Eq, Show)

instance Flag LineCapStyle where
    toCInt Butt     = _butt
    toCInt RoundCap = _round
    toCInt Square   = _square

-- | Sets how the end of the line (cap) is drawn.
lineCap :: LineCapStyle -> VG ()
lineCap style = applyContext $ \ptr -> c_lineCap ptr (toCInt style)

data LineJoinStyle =
      Miter       
    | RoundJoin   
    | Bevel       
    deriving (Eq, Show)

instance Flag LineJoinStyle where
    toCInt Miter     = _miter
    toCInt RoundJoin = _round
    toCInt Bevel     = _bevel


-- | Sets how sharp path corners are drawn.
lineJoin :: LineJoinStyle -> VG ()
lineJoin style = applyContext $ \ptr -> c_lineJoin ptr (toCInt style)

-- | Sets the transparency applied to all rendered shapes.
--   Already transparent paths will get proportionally more transparent as well.
globalAlpha :: Float -> VG ()
globalAlpha alpha = applyContext $ \ptr -> c_globalAlpha ptr (realToFrac alpha)

-- | Sets the miter limit of the stroke style.
--   Miter limit controls when a sharp corner is beveled.
miterLimit :: Float -> VG ()
miterLimit limit = applyContext $ \ptr -> c_miterLimit ptr (realToFrac limit)

-- | Draws current path as a series of strokes.
stroke :: VG ()
stroke = applyContext c_stroke

-- | Sets color of strokes
strokeColor :: Color
            -> VG ()
strokeColor Color{..} = 
    applyContext $ \ptr ->
        c_strokeColor ptr 
        (realToFrac _red) 
        (realToFrac _green) 
        (realToFrac _blue) 
        (realToFrac _alpha)


-- | Set width of strokes.
strokeWidth :: Float
            -> VG ()
strokeWidth width = applyContext $ \ptr -> c_strokeWidth ptr (realToFrac width)

-- | Fills area within current path with color.
fill :: VG ()
fill = applyContext c_fill

-- | Sets color of filling. 
fillColor :: Color
          -> VG ()
fillColor Color{..} = 
    applyContext $ \ptr ->
        c_fillColor ptr 
        (realToFrac _red) 
        (realToFrac _green) 
        (realToFrac _blue) 
        (realToFrac _alpha)



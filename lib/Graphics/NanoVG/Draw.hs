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

import Foreign.ForeignPtr
import Foreign.C.Types
--
import Linear.V2

import Graphics.NanoVG.Context
import Graphics.NanoVG.Color
import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.Draw



-- | Data type for compositing operations. Documentation for the compisiting operation shamelessly stolen from [https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Compositing] (cf there for pictures).
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

opToCint :: CompositeOp -> CInt
opToCint SourceOver      = _source_over
opToCint SourceIn        = _source_in
opToCint SourceOut       = _source_out
opToCint SourceAtop      = _source_atop
opToCint DestinationOver = _destination_over
opToCint DestinationIn   = _destination_in
opToCint DestinationOut  = _destination_out
opToCint DestinationAtop = _destination_atop
opToCint Lighter         = _lighter
opToCint Copy            = _copy
opToCint Xor             = _xor
opToCint _               = error "Custom operations do not have CInt representations."



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
                                        (opToCint op)


data LineCapStyle =
      Butt
    | RoundCap
    | Square
    deriving (Eq, Show)

capToCInt :: LineCapStyle -> CInt
capToCInt Butt     = _butt
capToCInt RoundCap = _round
capToCInt Square   = _square

-- | Sets how the end of the line (cap) is drawn
lineCap :: LineCapStyle -> VG ()
lineCap style = applyContext $ \ptr -> c_lineCap ptr (capToCInt style)

data LineJoinStyle =
      Miter
    | RoundJoin
    | Bevel
    deriving (Eq, Show)

joinToCInt :: LineJoinStyle -> CInt
joinToCInt Miter     = _miter
joinToCInt RoundJoin = _round
joinToCInt Bevel     = _bevel

-- | Sets how sharp path corners are drawn.
lineJoin :: LineJoinStyle -> VG ()
lineJoin style = applyContext $ \ptr -> c_lineJoin ptr (joinToCInt style)

-- | Sets the transparency applied to all rendered shapes.
--   Already transparent paths will get proportionally more transparent as well.
globalAlpha :: Float -> VG ()
globalAlpha alpha = applyContext $ \ptr -> c_globalAlpha ptr (realToFrac alpha)

-- | Sets the miter limit of the stroke style.
--   Miter limit controls when a sharp corner is beveled.
miterLimit :: Float -> VG ()
miterLimit limit = applyContext $ \ptr -> c_miterLimit ptr (realToFrac limit)

-- | Draws current path as a series of strokes
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


-- | Set width of strokes
strokeWidth :: Float
            -> VG ()
strokeWidth width = applyContext $ \ptr -> c_strokeWidth ptr (realToFrac width)

-- | Fills area within current path with color
fill :: VG ()
fill = applyContext c_fill

-- | Sets color of filling  
fillColor :: Color
          -> VG ()
fillColor Color{..} = 
    applyContext $ \ptr ->
        c_fillColor ptr 
        (realToFrac _red) 
        (realToFrac _green) 
        (realToFrac _blue) 
        (realToFrac _alpha)



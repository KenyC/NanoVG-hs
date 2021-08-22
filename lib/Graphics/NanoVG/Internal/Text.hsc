module Graphics.NanoVG.Internal.Text where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
--
import Control.Monad (ap)

#include "nanovg.h"
#include "nanovg_hs_wrapper.h"

-- | Creates font by loading it from the disk from specified file name.
--   Returns handle to the font.
foreign import ccall unsafe "nanovg.h nvgCreateFont"
    c_createFont :: Ptr ()
                 -> Ptr CChar -- ^ name
                 -> Ptr CChar -- ^ filename
                 -> IO CInt


-- | fontIndex specifies which font face to load from a .ttf/.ttc file.
foreign import ccall unsafe "nanovg.h nvgCreateFontAtIndex"
    c_createFontAtIndex :: Ptr ()
                        -> Ptr CChar
                        -> Ptr CChar
                        -> Int
                        -> IO CInt

-- | Creates font by loading it from the specified memory chunk.
--   Returns handle to the font.
foreign import ccall unsafe "nanovg.h nvgCreateFontMem"
    c_createFontMem :: Ptr ()
                    -> Ptr CUChar
                    -> CInt
                    -> CInt
                    -> IO CInt

-- | fontIndex specifies which font face to load from a .ttf/.ttc file.
foreign import ccall unsafe "nanovg.h nvgCreateFontMemAtIndex"
    c_createFontMemAtIndex ::
           Ptr ()
        -> IO CInt

-- | Finds a loaded font of specified name, and returns handle to it, or -1 if the font is not found.
foreign import ccall unsafe "nanovg.h nvgFindFont"
    c_findFont :: Ptr ()
               -> Ptr CChar
               -> IO CInt

-- | Adds a fallback font by handle.
foreign import ccall unsafe "nanovg.h nvgAddFallbackFontId"
    c_addFallbackFontId ::  Ptr ()
                         -> CInt
                         -> CInt
                         -> IO CInt

-- | Adds a fallback font by name.
foreign import ccall unsafe "nanovg.h nvgAddFallbackFont"
    c_addFallbackFont :: Ptr ()
                      -> Ptr CChar
                      -> Ptr CChar
                      -> IO CInt

-- | Resets fallback fonts by handle.
foreign import ccall unsafe "nanovg.h nvgResetFallbackFontsId"
    c_resetFallbackFontsId :: Ptr ()
                           -> CInt
                           -> IO ()

-- | Resets fallback fonts by name.
foreign import ccall unsafe "nanovg.h nvgResetFallbackFonts"
    c_resetFallbackFonts :: Ptr ()
                         -> Ptr CChar
                         -> IO ()

-- | Sets the font size of current text style.
foreign import ccall unsafe "nanovg.h nvgFontSize"
    c_fontSize :: Ptr ()
               -> CFloat
               -> IO ()

-- | Sets the blur of current text style.
foreign import ccall unsafe "nanovg.h nvgFontBlur"
    c_fontBlur :: Ptr ()
               -> CFloat
               -> IO ()

-- | Sets the letter spacing of current text style.
foreign import ccall unsafe "nanovg.h nvgTextLetterSpacing"
    c_textLetterSpacing :: Ptr ()
                        -> CFloat
                        -> IO ()

-- | Sets the proportional line height of current text style. The line height is specified as multiple of font size.
foreign import ccall unsafe "nanovg.h nvgTextLineHeight"
    c_textLineHeight :: Ptr ()
                     -> CFloat
                     -> IO ()

#{enum CInt,
    , _align_left     = NVG_ALIGN_LEFT          
    , _align_center   = NVG_ALIGN_CENTER      
    , _align_right    = NVG_ALIGN_RIGHT        
    , _align_top      = NVG_ALIGN_TOP            
    , _align_middle   = NVG_ALIGN_MIDDLE      
    , _align_bottom   = NVG_ALIGN_BOTTOM      
    , _align_baseline = NVG_ALIGN_BASELINE  
}                  

-- | Sets the text align of current text style, see NVGalign for options.
foreign import ccall unsafe "nanovg.h nvgTextAlign"
    c_textAlign :: Ptr ()
                -> CInt
                -> IO ()

-- | Sets the font face based on specified id of current text style.
foreign import ccall unsafe "nanovg.h nvgFontFaceId"
    c_fontFaceId :: Ptr ()
                 -> CInt
                 -> IO ()

foreign import ccall safe "nanovg_hs_wrapper.h &_defaultFontName"
    c_defaultFontName :: Ptr CChar

-- | Sets the font face based on specified name of current text style.
foreign import ccall unsafe "nanovg.h nvgFontFace"
    c_fontFace :: Ptr ()
               -> Ptr CChar
               -> IO ()

-- | Draws text string at specified location. If end is specified only the sub-string up to the end is drawn.
foreign import ccall unsafe "nanovg.h nvgText"
    c_text :: Ptr ()
           -> CFloat
           -> CFloat
           -> Ptr CChar
           -> Ptr CChar
           -> IO CFloat

-- | Draws multi-line text string at specified location wrapped at the specified width. If end is specified only the sub-string up to the end is drawn.
--   White space is stripped at the beginning of the rows, the text is split at word boundaries or when new-line characters are encountered.
--   Words longer than the max width are slit at nearest character (i.e. no hyphenation).
foreign import ccall unsafe "nanovg.h nvgTextBox"
    c_textBox :: Ptr ()
              -> CFloat
              -> CFloat
              -> CFloat
              -> Ptr CChar
              -> Ptr CChar
              -> IO ()

-- | Measures the specified text string. Parameter bounds should be a pointer to float[4],
--   if the bounding box of the text should be returned. The bounds value are [xmin,ymin, xmax,ymax]
--   Returns the horizontal advance of the measured text (i.e. where the next character should drawn).
--   Measured values are returned in local coordinate space.
foreign import ccall unsafe "nanovg.h nvgTextBounds"
    c_textBounds :: Ptr ()
                 -> CFloat
                 -> CFloat 
                 -> Ptr CChar
                 -> Ptr CChar 
                 -> Ptr CFloat 
                 -> IO CFloat

-- | Measures the specified multi-text string. Parameter bounds should be a pointer to float[4],
--   if the bounding box of the text should be returned. The bounds value are [xmin,ymin, xmax,ymax]
--   Measured values are returned in local coordinate space.
foreign import ccall unsafe "nanovg.h nvgTextBoxBounds"
    c_textBoxBounds :: Ptr ()
                    -> CFloat
                    -> CFloat
                    -> CFloat
                    -> Ptr CChar
                    -> Ptr CChar 
                    -> Ptr CFloat 
                    -> IO ()


-- foreign import ccall unsafe "nanovg.h nvgTextGlyphPositions"
--     -- | Calculates the glyph x positions of the specified text. If end is specified only the sub-string will be used.
--     --   Measured values are returned in local coordinate space.
--     c_textGlyphPositions ::
--            Ptr ()
--         -> CFloat
--         -> CFloat
--         -> Ptr CChar
--         -> Ptr CChar
--         -> 
--         -> IO CInt
--     (NVGcontext* ctx, float x, float y, const char* string, const char* end, NVGglyphPosition* positions, int maxPositions);

-- | Returns the vertical metrics based on the current text style.
--   Measured values are returned in local coordinate space.
foreign import ccall unsafe "nanovg.h nvgTextMetrics"
    c_textMetrics ::
           Ptr ()
        -> Ptr CFloat
        -> Ptr CFloat
        -> Ptr CFloat
        -> IO ()

-- | Breaks the specified text into lines. If end is specified only the sub-string will be used.
--   White space is stripped at the beginning of the rows, the text is split at word boundaries or when new-line characters are encountered.
--   Words longer than the max width are slit at nearest character (i.e. no hyphenation).
foreign import ccall unsafe "nanovg.h nvgTextBreakLines"
    c_textBreakLines :: Ptr ()
                     -> Ptr ()
                     -> Ptr ()
                     -> CFloat
                     -> Ptr CTextRow
                     -> CInt
                     -> IO CInt


foreign import ccall unsafe "nanovg_hs_wrapper.h nvgStartIterTextLines"
    c_startIterTextLines :: Ptr CChar
                         -> Ptr CChar
                         -> CFloat
                         -> IO (Ptr ())


foreign import ccall unsafe "nanovg_hs_wrapper.h nvgIterTextLines"
    c_iterTextLines :: Ptr ()
                    -> Ptr ()
                    -> Ptr CTextRow
                    -> IO CInt





data CTextRow = CTextRow {
    _start :: CInt,
    _end   :: CInt,
    _width :: CFloat,
    _minX  :: CFloat,
    _maxX  :: CFloat
} deriving (Show)

instance Storable CTextRow where
    sizeOf    _ = #{size      NVGtextRowHs}
    alignment _ = #{alignment NVGtextRowHs}

    poke p cTextRow = do
      #{poke NVGtextRowHs, start} p $ _start cTextRow
      #{poke NVGtextRowHs, end}   p $ _end   cTextRow
      #{poke NVGtextRowHs, width} p $ _width cTextRow
      #{poke NVGtextRowHs, minx}  p $ _minX  cTextRow
      #{poke NVGtextRowHs, maxx}  p $ _maxX  cTextRow

    peek p = return CTextRow
             `ap` (#{peek NVGtextRowHs, start} p)
             `ap` (#{peek NVGtextRowHs, end}   p)
             `ap` (#{peek NVGtextRowHs, width} p)
             `ap` (#{peek NVGtextRowHs, minx}  p)
             `ap` (#{peek NVGtextRowHs, maxx}  p)




foreign import ccall unsafe "nanovg_hs_wrapper.h nvgStartIterTextGlyph"
    c_startIterTextGlyph :: Ptr CChar
                         -> Ptr CChar
                         -> CFloat
                         -> CFloat
                         -> IO (Ptr ())


foreign import ccall unsafe "nanovg_hs_wrapper.h nvgIterTextGlyph"
    c_iterTextGlyph :: Ptr ()
                    -> Ptr ()
                    -> Ptr CTextGlyph
                    -> IO CInt




data CTextGlyph = CTextGlyph {
    _index     :: CInt,
    _logicalX  :: CFloat,
    _minXGlyph :: CFloat,
    _maxXGlyph :: CFloat
} deriving (Show)

instance Storable CTextGlyph where
    sizeOf    _ = #{size      NVGtextGlyphHs}
    alignment _ = #{alignment NVGtextGlyphHs}

    poke p cGlyphPos = do
      #{poke NVGtextGlyphHs, position} p $ _index      cGlyphPos
      #{poke NVGtextGlyphHs, x}        p $ _logicalX   cGlyphPos
      #{poke NVGtextGlyphHs, minx}     p $ _minXGlyph  cGlyphPos
      #{poke NVGtextGlyphHs, maxx}     p $ _maxXGlyph  cGlyphPos

    peek p = return CTextGlyph
             `ap` (#{peek NVGtextGlyphHs, position} p)
             `ap` (#{peek NVGtextGlyphHs, x}        p)
             `ap` (#{peek NVGtextGlyphHs, minx}     p)
             `ap` (#{peek NVGtextGlyphHs, maxx}     p)

{-|
Module      : Graphics.NanoVG.Text
Description : Creating font, rendering text
Copyright   : (c) Keny C, 2021
License     : MIT
Stability   : experimental

This module define functions to load fonts and render text to the screen.
Fonts are loaded from file via 'createFont'. Text is then rendered through one of 'text' or 'byteString' (one line text), 'textBox' or 'byteStringBox' (multi-line text). 
Every function that asks for 'ByteString' argument expects them to represent UTF-8 encoded text.

NB: functions calling for 'ByteString' arguments are more efficient, because they don't require copying the data.
-}
module Graphics.NanoVG.Text (
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
    FontMetrics(..),
    fontMetrics,
    TextRow(..),
    byteStringBreakLines,
    GlyphPosition(..),
    addFallbackFont,
    resetFallbackFont,
    textGlyphPos,
    byteStringGlyphPos
) where

import Control.Exception (bracket)
import Control.Monad     (ap, void)
--
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable      hiding (alignment)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
--
import Linear.V2
--
import Data.Text (Text)
import qualified Data.Text         as Text
import qualified Data.Text.Foreign as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString        as BS (null)
import qualified Data.ByteString.Unsafe as BS
import Data.Bits ((.|.))

import Graphics.NanoVG.Context
import Graphics.NanoVG.Internal.Text hiding (CTextRow(..))
import qualified Graphics.NanoVG.Internal.Text as Internal
import Graphics.NanoVG.Internal.Flag


-- | Data type for fonts
newtype Font = Font {
    _fontId :: CInt
} deriving (Show)


-- | Loads font at given file path (.ttf only). Returns 'Nothing' if file could not be loaded.
createFont :: FilePath -> VG (Maybe Font)
createFont filenameHs = 
    applyContext $ \ptr -> 
        withCString filenameHs $ \filename -> do
            fontId <- c_createFont ptr c_defaultFontName filename
            return $ case fontId of 
                (-1) -> Nothing
                _    -> Just $ Font fontId

-- | Sets font for subsequent text writing.
fontFace :: Font
         -> VG ()
fontFace (Font fontId) = applyContext $ \ptr -> c_fontFaceId ptr fontId


-- | Sets font size for subsequent text writing.
fontSize :: Float -- ^ size
         -> VG ()
fontSize size = applyContext $ \ptr -> c_fontSize ptr (realToFrac size)


-- | Sets font blur for subsequent text writing.
fontBlur :: Float -- ^ blur (in px)
         -> VG ()
fontBlur size = applyContext $ \ptr -> c_fontBlur ptr (realToFrac size)


-- | Horizontal alignment
data HAlign = LeftAlign      -- ^ Default, align text horizontally to left.
            | CenterAlign    -- ^ Align text horizontally to center. 
            | RightAlign     -- ^ Align text horizontally to right.
            deriving (Show, Eq)

instance Flag HAlign where
    toCInt LeftAlign   = _align_left
    toCInt RightAlign  = _align_right
    toCInt CenterAlign = _align_center

-- | Vertical alignment
data VAlign = Top       -- ^ Align text vertically to top. 
            | Middle    -- ^ Align text vertically to middle. 
            | Bottom    -- ^ Align text vertically to bottom. 
            | Baseline  -- ^ Default, align text vertically to baseline. 
            deriving (Show, Eq)
instance Flag VAlign where
    toCInt Top      = _align_top
    toCInt Middle   = _align_middle
    toCInt Bottom   = _align_bottom
    toCInt Baseline = _align_baseline

-- | Horizontal and vertical alignment
data Align = Align !HAlign !VAlign deriving (Show, Eq)

instance Flag Align where
    toCInt (Align hAlign vAlign) = (toCInt hAlign) .|. (toCInt vAlign)

-- | Sets alignment properties for subsequent text writing.
textAlign :: Align -> VG ()
textAlign alignment = applyContext $ \ptr -> c_textAlign ptr (toCInt alignment)

-- | Sets space between letters (in px).
textLetterSpacing :: Float -> VG ()
textLetterSpacing space = applyContext $ \ptr -> c_textLetterSpacing ptr (realToFrac space)

-- | Sets space between two lines (as multiple of font size).
textLineHeight :: Float -> VG ()
textLineHeight space = applyContext $ \ptr -> c_textLineHeight ptr (realToFrac space)

-- | Add fallback font to given font
addFallbackFont :: Font     -- ^ base font
                -> Font     -- ^ font to fall back to if base font is not unavailable
                -> VG Bool  -- ^ returns true if sucessful
addFallbackFont (Font baseId) (Font fallbackId) = applyContext $ \ptr -> do
    isSuccess <- c_addFallbackFontId ptr baseId fallbackId
    return $ isSuccess /= 0

-- | Removes fallback font of given font
resetFallbackFont :: Font -- ^ font to remove fallback font from
                  -> VG ()
resetFallbackFont (Font fontId) = applyContext $ \ptr ->
    c_resetFallbackFontsId ptr fontId

-- | Writes text at given location. 
text :: V2 Float -- ^ where to write text
     -> Text     -- ^ what to write
     -> VG Float -- ^ horizontal advance of text (i.e. where a potential next character should be drawn)
text (V2 x y) contents 
    | Text.null contents = return x -- Segfaults if not (not sure why)
    | otherwise          = applyContext $ \ptr ->
                             Text.withCStringLen contents $ \(contentsC, size) -> do
                                 realToFrac <$> c_text 
                                     ptr 
                                     (realToFrac x) (realToFrac y)
                                     contentsC
                                     (plusPtr contentsC $ size * sizeOf (undefined :: CChar))

-- | Writes text UTF-8 encoded as ByteString at given location. ByteString equivalent of 'text'. 
--   This function is more efficient as it does not involve copying data.
byteString :: V2 Float   -- ^ where to write text
           -> ByteString -- ^ what to write  (ByteString representing text as UTF-8)
           -> VG Float   -- ^ horizontal advance of text (i.e. where a potential next character should be drawn)
byteString (V2 x y) contents 
    | BS.null contents = return x   -- NanoVG functions don't check for null pointers ; occasionally an empty ByteString is associated to a null pointer.
    | otherwise        = applyContext $ \ptr -> 
        BS.unsafeUseAsCStringLen contents $ \(contentsC, size) -> do
            realToFrac <$> c_text 
                ptr 
                (realToFrac x) (realToFrac y)
                contentsC
                (plusPtr contentsC $ size * sizeOf (undefined :: CChar))


-- | Writes text at given location. If text is longer than provided width, text is broken in multiple lines at word boundaries. 
--   If this is not possible because some word is longer than width, this word is broken at the character nearest to end of the line.
textBox :: V2 Float   -- ^ where to write text
        -> Float      -- ^ width of text
        -> Text       -- ^ what to write
        -> VG ()
textBox (V2 x y) width contents = applyContext $ \ptr -> 
    Text.withCStringLen contents $ \(contentsC, size) -> do
        c_textBox
            ptr 
            (realToFrac x) (realToFrac y)
            (realToFrac width)
            contentsC
            (plusPtr contentsC $ size * sizeOf (undefined :: CChar))


-- | Writes text UTF-8 encoded as ByteString at given location. ByteString equivalent of 'textBox'. 
--   This function is more efficient as it does not involve copying data.
byteStringBox :: V2 Float   -- ^ where to write text
              -> Float      -- ^ width of text
              -> ByteString -- ^ what to write (ByteString representing text as UTF-8)
              -> VG ()
byteStringBox (V2 x y) width contents
    | BS.null contents = return ()   -- NanoVG functions don't check for null pointers ; occasionally an empty ByteString is associated to a null pointer.
    | otherwise        = applyContext $ \ptr -> 
        BS.unsafeUseAsCStringLen contents $ \(contentsC, size) -> do
            c_textBox
                ptr 
                (realToFrac x) (realToFrac y)
                (realToFrac width)
                contentsC
                (plusPtr contentsC $ size * sizeOf (undefined :: CChar))



-- | Returns position and size of the smallest box containing the text written by the corresponding call to 'text'.
textBounds :: V2 Float                -- ^ text position
           -> Text                    -- ^ text
           -> VG (V2 Float, V2 Float) -- ^ (position top left corner, width x height)
textBounds (V2 x y) contents = applyContext $ \ptr -> do
    Text.withCStringLen contents $ \(contentsC, size) -> do
        withArray [0, 0, 0, 0] $ \bounds -> do
            void $ c_textBounds
                ptr 
                (realToFrac x) (realToFrac y)
                contentsC
                (plusPtr contentsC $ size * sizeOf (undefined :: CChar))    
                bounds
            boundsList <- peekArray 4 bounds 
            let xMin:yMin:xMax:yMax:_ = map realToFrac boundsList
            return (V2 xMin yMin, V2 (xMax - xMin) (yMax - yMin))


-- | Returns position and size of the smallest box containing the text written by the corresponding call to 'textBox'.
textBoxBounds :: V2 Float                -- ^ text position
              -> Float                   -- ^ width
              -> Text                    -- ^ text
              -> VG (V2 Float, V2 Float) -- ^ (position top left corner, width x height)
textBoxBounds (V2 x y) width contents = applyContext $ \ptr -> do
    Text.withCStringLen contents $ \(contentsC, size) -> do
        withArray [0, 0, 0, 0] $ \bounds -> do
            c_textBoxBounds
                ptr 
                (realToFrac x) (realToFrac y)
                (realToFrac width)
                contentsC
                (plusPtr contentsC $ size * sizeOf (undefined :: CChar))    
                bounds
            boundsList <- peekArray 4 bounds 
            let xMin:yMin:xMax:yMax:_ = map realToFrac boundsList
            return (V2 xMin yMin, V2 (xMax - xMin) (yMax - yMin))



-- | Returns position and size of the smallest box containing the text written by the corresponding call to 'byteString'.
byteStringBounds :: V2 Float                -- ^ text position
                 -> ByteString              -- ^ text
                 -> VG (V2 Float, V2 Float) -- ^ (position top left corner, width x height)
byteStringBounds pos@(V2 x y) contents
    | BS.null contents = return (pos, 0)
    | otherwise        = applyContext $ \ptr -> do
        BS.unsafeUseAsCStringLen contents $ \(contentsC, size) -> do
            withArray [0, 0, 0, 0] $ \bounds -> do
                void $ c_textBounds
                    ptr 
                    (realToFrac x) (realToFrac y)
                    contentsC
                    (plusPtr contentsC $ size * sizeOf (undefined :: CChar))    
                    bounds
                boundsList <- peekArray 4 bounds 
                let xMin:yMin:xMax:yMax:_ = map realToFrac boundsList
                return (V2 xMin yMin, V2 (xMax - xMin) (yMax - yMin))


-- | Returns position and size of the smallest box containing the text written by the corresponding call to 'byteStringBox'.
byteStringBoxBounds :: V2 Float                -- ^ text position
                    -> Float                   -- ^ width
                    -> ByteString              -- ^ text
                    -> VG (V2 Float, V2 Float) -- ^ (position top left corner, width x height)
byteStringBoxBounds pos@(V2 x y) width contents
    | BS.null contents = return (pos, 0)
    | otherwise        = applyContext $ \ptr -> do
        BS.unsafeUseAsCStringLen contents $ \(contentsC, size) -> do
            withArray [0, 0, 0, 0] $ \bounds -> do
                void $ c_textBoxBounds
                    ptr 
                    (realToFrac x) (realToFrac y)
                    (realToFrac width)
                    contentsC
                    (plusPtr contentsC $ size * sizeOf (undefined :: CChar))    
                    bounds
                boundsList <- peekArray 4 bounds 
                let xMin:yMin:xMax:yMax:_ = map realToFrac boundsList
                return (V2 xMin yMin, V2 (xMax - xMin) (yMax - yMin))


-- | Information about a font's metric characteristic.
data FontMetrics = FontMetrics {
    _ascending  :: !Float,  -- ^ how much a letter can go above the baseline 
    _descending :: !Float,  -- ^ how much a letter can go below the baseline (letters with tails like j, g)
    _lineHeight :: !Float   -- ^ total line height
} deriving (Eq, Show)


-- | Get the metrics of the current font.
fontMetrics :: VG FontMetrics
fontMetrics = applyContext $ \ptr -> do
    alloca $ \ascendingC   -> do
     alloca $ \descendingC  -> do
      alloca $ \lineHeightC  -> do
        c_textMetrics ptr ascendingC descendingC lineHeightC
        
        (return FontMetrics) 
            `ap` (realToFrac <$> peek ascendingC)
            `ap` (realToFrac <$> peek descendingC)
            `ap` (realToFrac <$> peek lineHeightC)

-- | This data type stores information about the position and dimensions of a particular line of text.
data TextRow = TextRow {
    _startIndex :: Int,    -- ^ index where the given row starts in the bytestring
    _endIndex   :: Int,    -- ^ index where the given row ends in the bytestring
    _width      :: Float,  -- ^ line width
    _minX       :: Float,  -- ^ min X of line   
    _maxX       :: Float   -- ^ max X of line
} deriving (Show, Eq)


-- | Breaks a text (UTF-8 encoded bytestring) into lines of maximum width 
--   The values returned are the same values used by 'byteStringBox' and 'textBox' in laying out the Text
byteStringBreakLines :: ByteString
                     -> Float
                     -> VG [TextRow]
byteStringBreakLines contents width
    | BS.null contents = return []
    | otherwise        = applyContext $ \ptr -> BS.unsafeUseAsCStringLen contents $ \(textC, len) -> do
        let withIter = bracket before after
            before   = c_startIterTextLines 
                            textC
                            (plusPtr textC $ len * sizeOf (undefined :: CChar)) 
                            (realToFrac width)
            after    = free
        alloca $ \row ->
            withIter $ \iter -> do

                let loop = do
                            notDone <- c_iterTextLines ptr iter row
                            if notDone == 0
                            then return []
                            else do
                                Internal.CTextRow start end widthLine minX maxX <- peek row
                                let item = TextRow 
                                                (fromIntegral start) 
                                                (fromIntegral end) 
                                                (realToFrac   widthLine) 
                                                (realToFrac   minX) 
                                                (realToFrac   maxX)
                                (item:) <$> loop
                loop


-- | This data type stores information about the position and dimensions of a particular glyph in the text.
data GlyphPosition = GlyphPosition {
      _index     :: Int    -- ^ index in of glyph's starting byte in original byte string
    , _logicalX  :: Float  -- ^ logical x position of glyph
    , _minXGlyph :: Float  -- ^ minimum x position of glyph
    , _maxXGlyph :: Float  -- ^ maximum x position of glyph
} deriving (Eq, Show)

-- | Returns the position the the glyphs of a given text would have if it were drawn using 'text' or 'byteString'.
byteStringGlyphPos :: ByteString         -- ^ text (UTF-8 encoded)
                   -> V2 Float           -- ^ position of text
                   -> VG [GlyphPosition] -- ^ the glyphs' position
byteStringGlyphPos contents (V2 posX posY) 
    | BS.null contents = return []
    | otherwise = applyContext $ \ptr -> BS.unsafeUseAsCStringLen contents $ \(textC, len) -> do
        let withIter = bracket before after
            before   = c_startIterTextGlyph 
                            textC
                            (plusPtr textC $ len * sizeOf (undefined :: CChar)) 
                            (realToFrac posX)
                            (realToFrac posY)
            after    = free
        alloca $ \glyph ->
            withIter $ \iter -> do

                let loop = do
                            notDone <- c_iterTextGlyph ptr iter glyph
                            if notDone == 0
                            then return []
                            else do
                                Internal.CTextGlyph index logicalX minX maxX <- peek glyph
                                let item = GlyphPosition 
                                                (fromIntegral index) 
                                                (realToFrac   logicalX) 
                                                (realToFrac   minX) 
                                                (realToFrac   maxX)
                                (item:) <$> loop
                loop

-- | Returns the position the the glyphs of a given text would have if it were drawn using 'text' or 'byteString'.
textGlyphPos :: Text               -- ^ text
             -> V2 Float           -- ^ position of text
             -> VG [GlyphPosition] -- ^ the glyphs' position
textGlyphPos contents (V2 posX posY) 
    | Text.null contents = return []
    | otherwise = applyContext $ \ptr -> Text.withCStringLen contents $ \(textC, len) -> do
        let withIter = bracket before after
            before   = c_startIterTextGlyph 
                            textC
                            (plusPtr textC $ len * sizeOf (undefined :: CChar)) 
                            (realToFrac posX)
                            (realToFrac posY)
            after    = free
        alloca $ \glyph ->
            withIter $ \iter -> do

                let loop = do
                            notDone <- c_iterTextGlyph ptr iter glyph
                            if notDone == 0
                            then return []
                            else do
                                Internal.CTextGlyph index logicalX minX maxX <- peek glyph
                                let item = GlyphPosition 
                                                (fromIntegral index) 
                                                (realToFrac   logicalX) 
                                                (realToFrac   minX) 
                                                (realToFrac   maxX)
                                (item:) <$> loop
                loop


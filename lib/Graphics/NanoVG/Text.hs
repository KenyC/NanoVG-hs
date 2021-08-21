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
    textBoxBounds
) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
--
import Linear.V2
--
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Bits ((.|.))

import Graphics.NanoVG.Context
import Graphics.NanoVG.Color
import Graphics.NanoVG.Internal
import Graphics.NanoVG.Internal.Text


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
hAlignToCint :: HAlign -> CInt
hAlignToCint LeftAlign   = _align_left
hAlignToCint RightAlign  = _align_right
hAlignToCint CenterAlign = _align_center

-- | Vertical alignment
data VAlign = Top       -- ^ Align text vertically to top. 
            | Middle    -- ^ Align text vertically to middle. 
            | Bottom    -- ^ Align text vertically to bottom. 
            | Baseline  -- ^ Default, align text vertically to baseline. 
            deriving (Show, Eq)
vAlignToCint :: VAlign -> CInt
vAlignToCint Top      = _align_top
vAlignToCint Middle   = _align_middle
vAlignToCint Bottom   = _align_bottom
vAlignToCint Baseline = _align_baseline

data Align = Align !HAlign !VAlign deriving (Show, Eq)
alignToCint :: Align -> CInt
alignToCint (Align hAlign vAlign) = (hAlignToCint hAlign) .|. (vAlignToCint vAlign)

-- | Sets alignment properties for subsequent text writing
textAlign :: Align -> VG ()
textAlign alignment = applyContext $ \ptr -> c_textAlign ptr (alignToCint alignment)

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
text (V2 x y) contents = applyContext $ \ptr -> 
    Text.withCStringLen contents $ \(contentsC, size) -> do
        realToFrac <$> c_text 
            ptr 
            (realToFrac x) (realToFrac y)
            contentsC
            (plusPtr contentsC $ size * sizeOf (undefined :: CChar))

-- | Writes text UTF-8 encoded as ByteString at given location. ByteString equivalent for 'text'. 
--   This function is more efficient as it does not involve copying data.
byteString :: V2 Float   -- ^ where to write text
           -> ByteString -- ^ what to write  (ByteString representing text as UTF-8)
           -> VG Float   -- ^ horizontal advance of text (i.e. where a potential next character should be drawn)
byteString (V2 x y) contents = applyContext $ \ptr -> 
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


-- | Writes text UTF-8 encoded as ByteString at given location. ByteString equivalent for 'textBox'. 
--   This function is more efficient as it does not involve copying data.
byteStringBox :: V2 Float   -- ^ where to write text
              -> Float      -- ^ width of text
              -> ByteString -- ^ what to write (ByteString representing text as UTF-8)
              -> VG ()
byteStringBox (V2 x y) width contents = applyContext $ \ptr -> 
    BS.unsafeUseAsCStringLen contents $ \(contentsC, size) -> do
        c_textBox
            ptr 
            (realToFrac x) (realToFrac y)
            (realToFrac width)
            contentsC
            (plusPtr contentsC $ size * sizeOf (undefined :: CChar))



-- | Returns position and size of the smallest box containing the text written by the corresponding call to 'text' or 'byteString'.
textBounds :: V2 Float                -- ^ text position
           -> Text                    -- ^ text
           -> VG (V2 Float, V2 Float) -- ^ (position top left corner, width x height)
textBounds (V2 x y) text = applyContext $ \ptr -> do
    Text.withCStringLen text $ \(contentsC, size) -> do
        withArray [0, 0, 0, 0] $ \bounds -> do
            c_textBounds
                ptr 
                (realToFrac x) (realToFrac y)
                contentsC
                (plusPtr contentsC $ size * sizeOf (undefined :: CChar))    
                bounds
            boundsList <- peekArray 4 bounds 
            let xMin:yMin:xMax:yMax:_ = map realToFrac boundsList
            return (V2 xMin yMin, V2 (xMax - xMin) (yMax - yMin))


-- | Returns position and size of the smallest box containing the text written by the corresponding call to 'textBox' or 'byteStringBox'.
textBoxBounds :: V2 Float                -- ^ text position
              -> Float                   -- ^ width
              -> Text                    -- ^ text
              -> VG (V2 Float, V2 Float) -- ^ (position top left corner, width x height)
textBoxBounds (V2 x y) width text = applyContext $ \ptr -> do
    Text.withCStringLen text $ \(contentsC, size) -> do
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


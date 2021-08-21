{-# LANGUAGE OverloadedStrings #-}
module Icons where

import Data.ByteString (ByteString)
import qualified Data.ByteString    as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text


------------------- ICONS -----------------

iconSearch :: ByteString
iconSearch  = Text.encodeUtf8 "\x1F50D"

iconCircledCross :: ByteString
iconCircledCross  = Text.encodeUtf8 "\x2716"

iconChevronRight :: ByteString
iconChevronRight  = Text.encodeUtf8 "\xE75E"

iconCheck :: ByteString
iconCheck  = Text.encodeUtf8 "\x2713"

iconLogin :: ByteString
iconLogin  = Text.encodeUtf8 "\xE740"

iconTrash :: ByteString
iconTrash  = Text.encodeUtf8 "\xE729"

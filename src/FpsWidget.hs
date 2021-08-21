{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FpsWidget where

import Control.Monad
--
import Text.Printf
--
import Graphics.NanoVG.Context
import Graphics.NanoVG.Color
import Graphics.NanoVG.Path
import Graphics.NanoVG.Draw
import Graphics.NanoVG.Text
import Graphics.NanoVG.Transform
--
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as Vector
import qualified Data.Text   as Text
--
import Linear.V2
--

historyCount :: Int
historyCount = 100

data FpsGraph = FpsGraph {
    _values :: Vector Float,
    _head   :: Int
}

emptyGraph :: FpsGraph
emptyGraph = FpsGraph {
    -- _values = Vector.replicate historyCount 0,
    _values = Vector.replicate historyCount 10,
    _head   = 0
}

updateGraph :: FpsGraph -> Float -> FpsGraph
updateGraph FpsGraph{..} value = FpsGraph {
    _values  = _values // [(newHead, value)],
    _head    = newHead
} where newHead = (_head + 1) `mod` historyCount

graphAvg :: FpsGraph -> Float
graphAvg (FpsGraph values _) = (Vector.sum values) / (fromIntegral $ Vector.length values)

drawGraph :: FpsGraph 
          -> V2 Float 
          -> Font
          -> VG ()
drawGraph graph@FpsGraph{..} position font = do
    let width   = 200
        height  = 35
        size    = V2 width height
        average = graphAvg graph



    -- frame
    withPath Open $ do
        rect position size
        fillColor $ fromRGBA 0 0 0 128
        fill

    -- graph
    withNewState $ do
        translate $ position + V2 0 height
        withPath Closed $ do
            moveTo 0
            forM_ [0 .. historyCount - 1] $ \i -> do
                -- let iFloat :: Float = fromIntegral i
                let value = min 100 $ (_values ! ((_head + i) `mod` historyCount))
                -- let value = 10
                lineTo $ V2 
                            (width * (fromIntegral i) / (fromIntegral historyCount - 1))
                            (- value / 100 * height)

            lineTo $ V2 width 0

        fillColor $ fromRGBA 255 192 0 128
        fill

    -- graph text
    fontFace font
    fontSize 15
    textAlign $ Align RightAlign Top
    fillColor $ fromRGBA 240 240 240 192

    void $ text
        (position + V2 (width - 3) 3)
        (Text.pack $ printf "%.2f ms" average)
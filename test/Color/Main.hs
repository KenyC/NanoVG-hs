{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Graphics.NanoVG.Color


main :: IO ()
main = do
    defaultMain $ testGroup 
                    "tests" 
                    [ allBetweenZeroOne 
                    , periodic         
                    , testPoints        ]

    return ()


allBetweenZeroOne :: TestTree 
allBetweenZeroOne = testProperty "All values between 0 and 1" $ \(hue :: Float) (saturation :: Float) (luminance :: Float) (alpha_ :: Float) -> 
    let Color red green blue alpha = fromHSLA hue saturation luminance alpha_ in
    red <= 1  && red >= 0  && green <= 1 && green >= 0 &&
    blue <= 1 && blue >= 0 && alpha <= 1 && alpha >= 0 

periodic :: TestTree 
periodic = testProperty "Zero hue same as one hue" $ \(saturation :: Float) (luminance :: Float) (alpha_ :: Float) -> 
    fromHSLA 0 saturation luminance alpha_  == fromHSLA 1 saturation luminance alpha_ 

testPoints :: TestTree
testPoints = testCaseSteps "Test particular values" $ \step -> do
    fromHSLA 0.5 0 0 1        @?= Color 0 0 0 1
    fromHSLA 0.5 0 1 1        @?= Color 1 1 1 1
    fromHSLA 0.5   1 0.5 1    @?= Color 1 0 1 1
    -- fromHSLA 0   1 0.5 1      @?= Color 1 0 1 1
    -- fromHSLA 1   1 0.5 1      @?= Color 1 0 1 1
    -- fromHSLA (1/3) 0.5 0.5 1 @?= Color 0.5 0.75 0.25 1
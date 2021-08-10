module Main (main) where

import Control.Monad
    ( unless
    , when
    )
import Data.Maybe
    ( fromJust
    , fromMaybe
    )
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
    ( Args
    , UserHooks
    , buildHook
    , confHook
    , defaultMainWithHooks
    , postClean
    , postConf
    , preConf
    , simpleUserHooks
    )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , configFlags
    , localPkgDescr
    )
import Distribution.Simple.Setup
    ( BuildFlags
    , CleanFlags
    , ConfigFlags
    , buildVerbosity
    , cleanVerbosity
    , configConfigurationsFlags
    , configVerbosity
    , fromFlag
    )
import System.Directory (getCurrentDirectory)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
           -- confHook  = undefined
           confHook  = localLibConfHook
           -- preConf   = undefined,
           -- postConf  = undefined
           -- buildHook = undefined
           -- postClean = undefined
       }




localLibConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) ->
                    ConfigFlags ->
                    IO LocalBuildInfo
localLibConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        library = fromJust $ PD.library packageDescription
        libraryBuildInfo = PD.libBuildInfo library
    currentDir <- getCurrentDirectory
    let newBuildInfo = localBuildInfo {
        localPkgDescr = packageDescription {
            PD.library = Just $ library {
                PD.libBuildInfo = libraryBuildInfo {
                    PD.includeDirs  = (currentDir ++ "/nanovg/include/"):PD.includeDirs libraryBuildInfo,
                    PD.extraLibDirs = (currentDir ++ "/nanovg/lib"):PD.extraLibDirs libraryBuildInfo
                }
            }
        }
    }
    -- undefined
    return newBuildInfo

-- import Distribution.Simple
-- main = defaultMain


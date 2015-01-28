{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (rawSystem)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
    [packagequalifier] <- getArgs
    currentdirectory <- getCurrentDirectory
    writeFile (currentdirectory </> "packages.db") "[]"
    exitCode <- rawSystem "cabal" [
        "install","--reinstall","--force-reinstalls",
        "--disable-library-profiling",
        "--with-ghc=haskell-modules",
        "--prefix=" ++ currentdirectory,
        "--package-db=" ++ (currentdirectory </> "packages.db"),
        "-v2",
        packagequalifier]
    print exitCode

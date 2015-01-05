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
        "install","--force-reinstalls",
        "--gcc-option=-I/usr/lib/ghc/include",
        "--haskell-suite","-w","haskell-modules",
        "--prefix=" ++ currentdirectory,
        "--package-db=" ++ (currentdirectory </> "packages.db"),
        "-v3",
        packagequalifier]
    print exitCode

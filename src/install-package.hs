{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (rawSystem)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
    packagequalifier <- getArgs
    currentdirectory <- getCurrentDirectory
    writeFile (currentdirectory </> "packages.db") "[]"
    exitCode <- rawSystem "cabal" ([
        "install",
        "-v2",
        "--ghc-pkg-options=--global-package-db=/home/pschuster/Projects/haskell-modules/installed_packages",
        "--disable-library-profiling",
        "--with-ghc=haskell-modules"] ++
        packagequalifier)
    print exitCode

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (FilePath)

import Turtle (
    Text,proc,empty,mkdir,cd,
    realpath,(<>),toText,FilePath)


main :: IO ()
main = do

    packageDbPath <- realpath "builtin_packages"
    haskellModulesPath <- realpath ".cabal-sandbox/bin/haskell-modules"

    proc "rm" ["-r", "-f", "packages"] empty
    mkdir "packages"
    cd "packages"

    mkdir "modules"
    targetPath <- realpath "modules"

    proc "cabal" ["sandbox","init"] empty

    proc "cabal" ([
        "install",
        "-v2",
        "--ghc-pkg-options=--global-package-db=" <> filePathToText packageDbPath,
        "--disable-library-profiling",
        "--with-ghc=" <> filePathToText haskellModulesPath,
        "--ghc-option=--haskell-modules-target-path=" <> filePathToText targetPath]
        ++ packageNames)
        empty

    return ()

packageNames :: [Text]
packageNames = ["containers"]

filePathToText :: FilePath -> Text
filePathToText = either id id . toText

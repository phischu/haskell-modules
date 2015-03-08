{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (FilePath)

import Turtle (
    Text,proc,empty,mkdir,cd,rmtree,
    realpath,(<>),toText,FilePath,fromText)


main :: IO ()
main = do

    packageDbPath <- realpath "installed_packages"
    haskellModulesPath <- realpath ".cabal-sandbox/bin/haskell-modules"

    rmtree "packages"
    mkdir "packages"
    cd "packages"

    mkdir (fromText packageName)
    targetPath <- realpath (fromText packageName)

    proc "cabal" ["sandbox","init"] empty
    proc "cabal" [
        "install",
        "--ghc-pkg-options=--global-package-db=" <> filePathToText packageDbPath,
        "--disable-library-profiling",
        "--with-ghc=" <> filePathToText haskellModulesPath,
        "--ghc-option=--haskell-modules-target-path=" <> filePathToText targetPath,
        packageName]
        empty

    return ()

packageName :: Text
packageName = "containers"

filePathToText :: FilePath -> Text
filePathToText = either id id . toText

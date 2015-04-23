{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (FilePath)

import Turtle (
    Text,proc,empty,mkdir,cd,inproc,find,cp,
    has,text,rmtree,
    realpath,(<>),toText,FilePath,filename,
    fold,fromString,(</>))

import Control.Monad (forM)
import qualified Control.Foldl as Fold (head)
import Data.Text (unpack)


main :: IO ()
main = do

    proc "rm" ["-r", "-f", "builtin_packages"] empty
    mkdir "builtin_packages"
    Just ghcLibDir <- fold (inproc "ghc" ["--print-libdir"] empty) Fold.head

    let ghcGlobalPackagesPath = textToFilePath ghcLibDir </> "package.conf.d"

    forM ["base","builtin_rts","ghc-prim","integer-gmp"] (\builtinPackageName -> do
        Just builtinPackagePath <- fold (
            find
                (has (text builtinPackageName))
                ghcGlobalPackagesPath)
            Fold.head
        cp builtinPackagePath ("builtin_packages/" <> filename builtinPackagePath))

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
packageNames = ["vector"]

filePathToText :: FilePath -> Text
filePathToText = either id id . toText

textToFilePath :: Text -> FilePath
textToFilePath = fromString . unpack

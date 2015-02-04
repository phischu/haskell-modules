module Main where

import Prelude hiding (readFile)
import System.Environment (getArgs)
import System.Directory (
    doesFileExist,createDirectoryIfMissing,getDirectoryContents)
import System.Process (callProcess)
import System.FilePath ((</>),(<.>),dropFileName,takeFileName)
import Data.Char (isUpper)
import Control.Monad (forM_,filterM,guard)
import Data.List (intercalate,isSuffixOf)
import System.IO.Strict (readFile)

main :: IO ()
main = do

    args <- getArgs

    let targetPath = "/home/pschuster/Projects/demo/hey"

    case args of

        ("--make":argsAfterMake) -> do

            let moduleNames = filter (\arg -> isUpper (head arg)) argsAfterMake

                otherArgs = filter (\arg -> not (isUpper (head arg))) argsAfterMake

                searchPaths = do
                    ('-':'i':searchPath) <- args
                    return searchPath

                languageExtensions = do
                    ('-':'X':languageExtension) <- args
                    return languageExtension

            forM_ moduleNames (\moduleName -> do

                let relativeModuleFile = map (\c -> if c == '.' then '/' else c) moduleName <.> "hs"
                    targetFilePath = targetPath </> relativeModuleFile

                modulePath <- findModule searchPaths relativeModuleFile

                createDirectoryIfMissing True (dropFileName targetFilePath)

                putStrLn ("HASKELL MODULE: " ++ moduleName)

                callProcess "ghc" (concat [
                    ["-E",
                    "-optP","-P",
                    "-o",targetFilePath],
                    otherArgs,
                    [modulePath]])

                preprocessedFile <- readFile targetFilePath
                writeFile targetFilePath (languageExtensionsLine languageExtensions ++ preprocessedFile))

        ("-c":argsAfterC) -> do


            let cFilePaths = filter (".c" `isSuffixOf`) argsAfterC

            forM_ cFilePaths (\cFilePath -> do

                let cFileName = takeFileName cFilePath
                    targetFilePath = targetPath </> "cbits" </> cFileName

                createDirectoryIfMissing True (dropFileName targetFilePath)

                putStrLn ("C FILE: " ++ cFileName)

                cFile <- readFile cFilePath
                writeFile targetFilePath cFile)


            let includeFolders = do
                    ('-':'I':includeFolder) <- args
                    return includeFolder

            forM_ includeFolders (\includeFolder -> do

                includeFolderContents <- getDirectoryContents includeFolder

                let cHeaderFileNames = filter (".h" `isSuffixOf`) includeFolderContents

                forM_ cHeaderFileNames (\cHeaderFileName -> do

                    let targetFilePath = targetPath </> "include" </> cHeaderFileName

                    createDirectoryIfMissing True (dropFileName targetFilePath)

                    putStrLn ("C HEADER FILE: " ++ cHeaderFileName)

                    cHeaderFile <- readFile (includeFolder </> cHeaderFileName)
                    writeFile targetFilePath cHeaderFile))

        _ -> return ()

    callProcess "ghc" args

    return ()

type ModuleName = String

findModule :: [FilePath] -> ModuleName -> IO FilePath
findModule searchPaths relativeModulePath = do

    let potentialModuleFiles = do
            searchPath <- searchPaths
            guard (not (null searchPath))
            return (searchPath </> relativeModulePath)

    existingModuleFiles <- filterM doesFileExist potentialModuleFiles

    case existingModuleFiles of
        [] -> error "HASKELL MODULE ERROR: No module file found"
        [moduleFile] -> return moduleFile
        moduleFiles -> error ("HASKELL MODULE ERROR: Multiple module files found: " ++ show moduleFiles)

type LanguageExtension = String

languageExtensionsLine :: [LanguageExtension] -> String
languageExtensionsLine languageExtensions =
    "{-# LANGUAGE " ++ intercalate ", " languageExtensions ++ " #-}\n"

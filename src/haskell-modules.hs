module Main where

import Prelude hiding (readFile)
import System.Environment (getArgs)
import System.Directory (
    doesFileExist,createDirectoryIfMissing,getDirectoryContents)
import System.Process (callProcess)
import System.FilePath ((</>),(<.>),dropFileName,takeFileName)
import Data.Char (isUpper)
import Control.Monad (forM_,filterM,guard)
import Data.List (intercalate,isSuffixOf,isPrefixOf,stripPrefix)
import Data.Maybe (maybeToList)
import System.IO.Strict (readFile)

main :: IO ()
main = do

    argsWithTargetPath <- getArgs

    let args = do
            arg <- argsWithTargetPath
            guard (not (targetPathPrefix `isPrefixOf` arg))
            return arg
        targetPaths = do
            arg <- argsWithTargetPath
            maybeToList (stripPrefix targetPathPrefix arg)
        targetPath = case targetPaths of
            [t] -> t
            _ -> error "Not exactly one target path."

    case args of

        ("--make":argsAfterMake) -> do

            let moduleNames = filter (\arg -> isUpper (head arg) && not ('-' `elem` arg)) argsAfterMake

                otherArgs = filter (\arg -> not (isUpper (head arg))) argsAfterMake

                searchPaths = do
                    ('-':'i':searchPath) <- args
                    return searchPath

                languageExtensions = do
                    ('-':'X':languageExtension) <- args
                    return languageExtension

            forM_ moduleNames (\moduleName -> do

                moduleFilePath <- findModule searchPaths moduleName

                let targetFilePath = targetPath </> modulePath moduleName "hs"

                createDirectoryIfMissing True (dropFileName targetFilePath)

                putStrLn ("HASKELL MODULE: " ++ moduleName)

                callProcess "ghc" (concat [
                    ["-E",
                    "-optP","-P",
                    "-optL","-P",
                    "-o",targetFilePath],
                    otherArgs,
                    [moduleFilePath]])

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
type Suffix = String

findModule :: [FilePath] -> ModuleName -> IO FilePath
findModule searchPaths moduleName = do

    let potentialModuleFiles = do
            suffix <- ["hs","lhs"]
            let relativeModuleFile = modulePath moduleName suffix
            searchPath <- searchPaths
            guard (not (null searchPath))
            return (searchPath </> relativeModuleFile)

    existingModuleFiles <- filterM doesFileExist potentialModuleFiles

    case existingModuleFiles of
        [] -> error ("HASKELL MODULE ERROR: No module file found " ++ moduleName)
        [moduleFile] -> return moduleFile
        moduleFiles -> error ("HASKELL MODULE ERROR: Multiple module files found: " ++ show moduleFiles)

modulePath :: ModuleName -> Suffix -> FilePath
modulePath moduleName suffix = map (\c -> if c == '.' then '/' else c) moduleName <.> suffix

type LanguageExtension = String

languageExtensionsLine :: [LanguageExtension] -> String
languageExtensionsLine languageExtensions =
    "{-# LANGUAGE " ++ intercalate ", " languageExtensions ++ " #-}\n"

targetPathPrefix :: FilePath
targetPathPrefix = "--haskell-modules-target-path="

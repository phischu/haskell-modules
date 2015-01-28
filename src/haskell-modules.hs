module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Process (callProcess)
import System.FilePath ((</>),(<.>),dropFileName)
import Data.Char (isUpper)
import Control.Monad (forM_,filterM,guard)
import Data.List (intercalate)

main :: IO ()
main = do
    args <- getArgs
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

            putStrLn "COMPILING!"
            putStrLn (languageExtensionsLine languageExtensions)
            putStrLn (unlines moduleNames)

            writeFile "language_extensions" (languageExtensionsLine languageExtensions)

            forM_ moduleNames (\moduleName -> do

                let relativeModulePath = map (\c -> if c == '.' then '/' else c) moduleName <.> "hs"
                    targetPath = "/home/pschuster/Projects/demo/hey" </> relativeModulePath

                modulePath <- findModule searchPaths relativeModulePath

                createDirectoryIfMissing True (dropFileName targetPath)

                callProcess "ghc" (concat [
                    ["-E",
                    "-optP", "-P",
                    "-optP","-include",
                    "-optP","language_extensions",
                    "-o",targetPath],
                    otherArgs,
                    [modulePath]]))

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
    "{-# LANGUAGE " ++ intercalate ", " languageExtensions ++ " #-}"

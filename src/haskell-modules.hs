module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Process (rawSystem)
import System.FilePath ((</>),(<.>))
import Data.Char (isUpper)
import Control.Monad (forM,filterM,guard)

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
            putStrLn "COMPILING!"
            putStrLn (unlines moduleNames)
            exitCodes <- forM moduleNames (\moduleName -> do
                modulePath <- findModule searchPaths moduleName
                rawSystem "ghc" (concat [
                    ["-E","-o","/home/pschuster/Projects/demo/hey/" ++ moduleName],
                    otherArgs,
                    [modulePath]]))
            print exitCodes
        _ -> return ()
    rawSystem "ghc" args
    return ()

type ModuleName = String

findModule :: [FilePath] -> ModuleName -> IO FilePath
findModule searchPaths moduleName = do
    let potentialModuleFiles = do
            searchPath <- searchPaths
            guard (not (null searchPath))
            let modulePath = map (\c -> if c == '.' then '/' else c) moduleName
            return (searchPath </> modulePath <.> "hs")
    existingModuleFiles <- filterM doesFileExist potentialModuleFiles
    case existingModuleFiles of
        [] -> error "HASKELL MODULE ERROR: No module file found"
        [moduleFile] -> return moduleFile
        moduleFiles -> error ("HASKELL MODULE ERROR: Multiple module files found: " ++ show moduleFiles)

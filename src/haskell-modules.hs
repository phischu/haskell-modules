module Main where

import System.Environment (getArgs)
import System.Process (rawSystem)
import Data.Char (isUpper)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--make":argsAfterMake) -> do
            let moduleNames = filter (\arg -> isUpper (head arg)) argsAfterMake
                modulePaths = map (\moduleName -> moduleName ++ ".hs")
            putStrLn "COMPILING!"
            putStrLn (unlines moduleNames)
        _ -> return ()
    rawSystem "ghc" args
    return ()



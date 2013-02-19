module Main where

import Control.Monad
import System.Directory
import System.Environment
import System.FilePath

import PageParser (parseFile)
import RssPrinter (toRss)

getAllFiles root = do names <- clean `fmap` getDirectoryContents root
                      let paths = map (root </>) names
                      dirs <- filterM doesDirectoryExist paths
                      files <- filterM doesFileExist paths
                      deeper <- mapM getAllFiles dirs
                      return (files ++ concat deeper)

  where clean = filter (\fn -> fn /= "." && fn /= "..")

main = do [root] <- getArgs
          blog <- parseFile root
          putStrLn $ toRss blog

module Main where
import JMA
import Base
import Plotter.Simple

import System.Directory
import Data.Monoid
import Data.List
import Data.Function
import System.Environment (getArgs)

dataFiles :: FilePath -> IO [FilePath]
dataFiles d = do
    files <- listDirectory d
    return . map ((d <> "/") <>) . filter (".csv" `isSuffixOf`) $ files


main :: IO ()
main = do
    [loc] <- getArgs
    files <- dataFiles loc
    dat <- readRainfallCSVs files
    plot1 "log_plot_sort_by_rain.png" dat

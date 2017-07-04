module Plotter.Simple where
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.List
import Data.Function

import Base

plot0 :: FilePath -> [RainDatum] -> IO ()
plot0 f ds = toFile def f $ do
    plot $ line "daily rain"  [numbered]
    where
        numbered :: [(Int, Double)]
        numbered =
            zip [0..] . map _rain . sortBy (compare `on` _rain) $ ds

-- | Log
plot1 :: FilePath -> [RainDatum] -> IO ()
plot1 f ds = toFile def f $ do
    plot $ line "daily rain"  [numbered]
    where
        numbered :: [(Int, Double)]
        numbered =
            zip [0..] . map (logBase 10 . _rain) . filter ((>0) . _rain) . sortBy (compare `on` _rain) $ ds

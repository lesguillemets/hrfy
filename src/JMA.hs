module JMA ( readRainfallCSV
    , readRainfallCSVs ) where
import Base

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Data.Monoid
import Text.Read
import Data.Function
import Data.List

-- We now exactly how the files are formatted.
readRainfallCSV :: FilePath -> IO [RainDatum]
readRainfallCSV f = (mapMaybe readRain . drop 5 . BC.lines) <$> BC.readFile f

readRainfallCSVs :: [FilePath] -> IO [RainDatum]
readRainfallCSVs fs = joinData <$> mapM readRainfallCSV fs

-- We could use constraiant : data is sorted for each file, but didn't
joinData :: [[RainDatum]] -> [RainDatum]
joinData = uniq  . sortBy (flip compare) . concat
    where
        f d [] = [d]
        f d acc@(h:r) = if ((==) `on` _date) h d then acc else d:acc
        uniq = foldr f []

-- For now we do want our program to craaaash
readRain :: ByteString -> Maybe RainDatum
readRain l =
    let [d, r, n, q , nm] = BC.split ',' l
        in
    Rf <$>
        parseDate d <*>
        parse r <*>
        ((/= 0) <$> parse n) <*>
        parse q <*>
        parse nm

parse :: Read a => ByteString -> Maybe a
parse = readMaybe . BC.unpack

parseDate :: ByteString -> Maybe Date
parseDate c =
    let nums = map (fmap fst . BC.readInt) $ BC.split '/' c
        in
    case nums of
         [y,m,d] -> Date <$> y <*> m <*> d
         _ -> Nothing


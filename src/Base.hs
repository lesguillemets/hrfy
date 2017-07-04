module Base where
import Data.Monoid
import Text.Printf

data Date = Date { _y :: Int
                 , _m :: Int
                 , _d :: Int} deriving (Eq, Ord)
instance Show Date where
    show (Date y m d) = show y <> ('/': pf m) <> ('/': pf d)
        where
            pf = printf "%02d"

data RainDatum = Rf { _date :: Date
                    , _rain :: Double
                    , _noRain :: Bool
                    , _quality :: Int
                    , _norm :: Int} deriving (Eq, Ord)


instance Show RainDatum where
    show (Rf d r nr q nm) =
        show d <> ('\t': printf "%.1f" r) <> ('\t': show nr)

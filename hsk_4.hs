import Data.Ratio

seriesK :: Int -> [Rational]
seriesK k = ( 1 % 1 ) : x 1 where
    x n = ( 1 % toInteger(k ^ n) ) : x (n + 1)

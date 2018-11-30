import Data.Ratio
import Data.List
import Data.Char

revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun
fun (begin, end)
    | begin > end     = Nothing
    | minBound == end = Just (end, (succ begin, end))
    | otherwise       = Just (end, (begin, pred end))

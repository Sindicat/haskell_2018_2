import Data.List

circShiftL :: Int -> [a] -> [a]
circShiftL n xs = take (length xs) (drop skip (cycle xs)) where
    len = length xs
    skip = if n < 0 then len - (mod (abs n) len) else n

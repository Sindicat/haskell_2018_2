import Data.Monoid

indices :: [a] -> [(Integer, a)]
indices as = zip ([0..]) as

zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy as pre = map (\t -> if pre t then t else mempty t) as

triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum as bs cs = map (\(x,y,z) -> x + y + z) $ zip3 as bs cs

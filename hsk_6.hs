import Data.List
import Data.Monoid
import Data.Eq
import Data.Ord

-- из задания 5
newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Show)

superMerge :: Ord a => [a] -> [a] -> [a]
superMerge [] list         = list
superMerge list []         = list
superMerge first@(element1:list1) second@(element2:list2)
  | element1 < element2 = element1 : superMerge list1 second
  | otherwise           = element2 : superMerge first list2

instance Ord a => Semigroup (SortedList a) where
    (SortedList as) <> (SortedList bs) = SortedList (superMerge as bs)

instance Ord a => Monoid (SortedList a) where
        mempty = SortedList []

-- ФП 6
msort :: Ord a => [a] -> SortedList a
msort []        = SortedList []
msort [element] = SortedList [element]
msort list      = left <> right
                  where
                    left        = msort $ take middleIndex list
                    right       = msort $ drop middleIndex list
                    middleIndex = (length list) `div` 2

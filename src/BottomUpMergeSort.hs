module BottomUpMergeSort
  ( Sortable
  , add
  , new
  , sort
  ) where

import           Data.Foldable ( foldl' )
import           Numeric.Natural ( Natural )

-- | The Sortable data structure from Chapter 3 of PFDS. Provides amortized
-- O(log n) 'add' and amortized O(n) 'sort'. We don't need the 'less' function
-- that's passed around in the book version since we can add and 'Ord'
-- constraint to the contents.
data Sortable a =
    Sortable
        { size :: Natural
        , segments :: [[a]]
        }
    deriving ( Show )

new :: Sortable a
new = Sortable 0 []

add :: Ord a => Sortable a -> a -> Sortable a
add sortable x =
    Sortable
        (initialSize + 1)
        (addSegment [ x ] (segments sortable) initialSize)
  where
    initialSize = size sortable

    addSegment seg segs size
      | size `mod` 2 == 0 = seg : segs
      | otherwise =
          addSegment (merge seg . head $ segs) (tail segs) (size `div` 2)

sort :: Ord a => Sortable a -> [a]
sort = foldl' merge [] . segments

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) =
    if x < y
        then x : merge xs (y : ys)
        else y : merge (x : xs) ys

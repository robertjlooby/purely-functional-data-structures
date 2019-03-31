module BottomUpMergeSortSpec
  ( spec
  ) where

import           Data.Foldable ( foldl' )
import qualified Data.List as DL

import           BottomUpMergeSort
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    it "returns a sorted list" $ property $ \xs -> sort (foldl' add new xs)
        === DL.sort (xs :: [Integer])

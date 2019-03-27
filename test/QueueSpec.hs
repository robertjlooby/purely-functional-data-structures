module QueueSpec
  ( spec
  ) where

import           Prelude hiding ( head, tail )

import           Queue
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "isEmpty" $ do
        it "for an empty queue" $ do
            isEmpty empty `shouldBe` True

        it "for a non-empty queue" $ property $ \(NonEmpty xs) ->
            isEmpty (queue xs :: Queue Integer) === False

    describe "head" $ do
        it "for an empty queue" $ do
            head (empty :: Queue Integer) `shouldBe` Nothing

        it "for a non-empty queue" $ property $ \x xs ->
            head (queue (x : xs) :: Queue Integer) === Just x

    describe "tail" $ do
        it "for an empty queue" $ do
            tail (empty :: Queue Integer) `shouldBe` empty

        it "for a non-empty queue" $ property $ \x xs ->
            tail (queue (x : xs) :: Queue Integer) === queue xs

    describe "snoc" $ do
        it "adds to an empty queue" $ property $ \x ->
            snoc empty (x :: Integer) === queue [ x ]

    describe "snoc, tail, head integration" $ do
        it "snocing and taking the heads" $ property $ \xs ->
            let fullQueue = foldl snoc (empty :: Queue Integer) xs
                pop ( acc, q ) _ = ( head q : acc, tail q )
            in foldl pop ( [], fullQueue ) xs
               === ( Just <$> reverse xs, empty )

{-# LANGUAGE ScopedTypeVariables #-}

module HeapSpec where

import Test.Hspec
import Test.QuickCheck
import Heap
import Data.List (sort)

data HeapProperty h = HeapProperty Property

instance Testable (HeapProperty q) where
    property (HeapProperty p) = p

heapProperty :: forall h a . (Ord a, Show a, Heap h) => [a] -> HeapProperty (h a)
heapProperty xs = HeapProperty $ (toList . (fromList :: [a] -> h a)) xs === sort xs

spec :: Spec
spec = do 
    describe "empty" $ do
        it "returns Empty" $ do
            (empty :: LeftistHeap ()) `shouldBe` Empty

    describe "isEmpty" $ do
        describe "empty heap" $ do
            it "returns True" $ do
                isEmpty (Empty :: LeftistHeap ()) `shouldBe` True

        describe "non-empty heap" $ do
            it "returns False" $ do
                let heap = Tree 1 0 Empty Empty
                isEmpty heap `shouldBe` False
    
    describe "heap property" $ do
        it "maintains the heap invariant" $ do
            property (heapProperty :: ([Int] -> HeapProperty (BinomHeap Int)))

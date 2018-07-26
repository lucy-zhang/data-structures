{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module QueueSpec where

import Test.QuickCheck
import Test.Hspec
import Queue
import Data.List (foldl')
import Data.Proxy (Proxy(..))

pop :: BatchedQueue a -> (a, BatchedQueue a)
pop q = (Queue.head q, Queue.tail q)

data QueueProperty q = QueueProperty Property

instance Testable (QueueProperty q) where
    property :: (QueueProperty q) -> Property
    property (QueueProperty p) = p

emptyIsEmpty :: forall q a . (Queue q) => QueueProperty (q a)
emptyIsEmpty = QueueProperty $ property $ isEmpty (empty :: q a)

fifo :: forall q a . (Queue q, Eq a, Show a) => [a] -> QueueProperty (q a)
fifo list = QueueProperty $ toList (fromList list :: q a) === list

fromList :: forall q a . (Queue q) => [a] -> q a
fromList = foldl' (flip snoc) (empty :: q a)

toList :: forall q a . (Queue q) => q a -> [a]
toList q
    | isEmpty q = []
    | otherwise = (Queue.head q) : (toList $ Queue.tail q)

spec :: Spec
spec = do
    describe "empty and isEmpty" $ do
        it "returns an empty queue" $ do
            property (emptyIsEmpty :: QueueProperty (BatchedQueue Int))

    describe "fifo" $ do
        it "has the fifo property" $ do
            property (fifo :: [Int] -> QueueProperty (BatchedQueue Int))

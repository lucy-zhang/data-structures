module Queue where

import qualified Data.List as L (head)

class Queue q where
    empty :: q a
    isEmpty :: q a -> Bool
    snoc :: a -> q a -> q a
    head :: q a -> a
    tail :: q a -> q a

data BatchedQueue a = BatchedQueue [a] [a] deriving (Show, Eq)

instance Queue BatchedQueue where
    empty                         = BatchedQueue [] []
    isEmpty (BatchedQueue [] [])  = True
    isEmpty _                     = False

    snoc a (BatchedQueue [] [])   = BatchedQueue [a] []
    snoc a (BatchedQueue xs ys)   = BatchedQueue xs (a:ys)

    head (BatchedQueue (x:_) _)   = x

    tail (BatchedQueue (_:[]) ys) = BatchedQueue (reverse ys) []
    tail (BatchedQueue (_:xs) ys) = BatchedQueue (xs) ys


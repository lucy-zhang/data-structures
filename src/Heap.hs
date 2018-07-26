module Heap where

class Heap h where
    empty :: h a
    isEmpty :: h a -> Bool
    insert :: Ord a => a -> h a -> h a
    merge :: Ord a => h a -> h a -> h a
    findMin :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a

data LeftistHeap a =
    Empty
  | Tree Int a (LeftistHeap a) (LeftistHeap a)
    deriving (Show, Eq)

instance Heap LeftistHeap where
    empty = Empty

    isEmpty Empty = True
    isEmpty _ = False

    insert a x = merge (Tree 1 a Empty Empty) x

    merge Empty x = x
    merge x Empty = x
    merge x@(Tree _ ax lx rx) y@(Tree _ ay ly ry)
        | ax <= ay = leftistTree ax lx (merge rx y)
        | otherwise = leftistTree ay ly (merge ry x)
        where
            rank t = case t of
                Empty -> 0
                Tree r _ _ _ -> r
            leftistTree a t1 t2
                | r1 <= r2 = Tree (r2 + 1) a t1 t2
                | otherwise = Tree (r1 + 1) a t2 t1
                where r1 = rank t1
                      r2 = rank t2

    findMin (Tree _ a _ _) = a

    deleteMin (Tree _ a left right) = merge left right

data BinomTree a = BinomTree a [BinomTree a]
    deriving (Eq, Show)

newtype BinomHeap a = BinomHeap [BinomTree a]
    deriving (Eq, Show)

link :: (Ord a) => BinomTree a -> BinomTree a -> BinomTree a
link t1@(BinomTree a1 ts1) t2@(BinomTree a2 ts2)
    | a2 > a1 = BinomTree a1 (t2 : ts1)
    | otherwise = BinomTree a2 (t1 : ts2)

rank :: BinomTree a -> Int
rank (BinomTree _ ts) = length ts

insertTree :: (Ord a) => BinomTree a -> BinomHeap a -> BinomHeap a
insertTree t (BinomHeap []) = BinomHeap [t]
insertTree t (BinomHeap (t' : ts))
    | rank t < rank t' = BinomHeap (t : t' : ts)
    | otherwise = insertTree (link t t') (BinomHeap ts)

instance Heap BinomHeap where
    empty = BinomHeap []

    isEmpty (BinomHeap ts) = null ts

    merge h1@(BinomHeap ts1) h2@(BinomHeap ts2) = case (ts1, ts2) of
        (_, []) -> h1
        ([], _) -> h2
        (t1 : ts1', t2 : ts2')
            | rank t1 < rank t2 -> let BinomHeap m = merge (BinomHeap ts1') h2
                in BinomHeap (t1 : m)
            | rank t1 > rank t2 -> let BinomHeap m = merge h1 (BinomHeap ts2')
                in BinomHeap (t2 : m)
            | otherwise -> insertTree (link t1 t2) (merge (BinomHeap ts1') (BinomHeap ts2'))

    insert a = insertTree (BinomTree a [])

    findMin (BinomHeap ts) = minimum [a | BinomTree a _ <- ts]

    deleteMin h@(BinomHeap ts) = merge (BinomHeap (reverse children)) (BinomHeap remaining)
        where (t1, t2) = span (\(BinomTree a _) -> a /= findMin h) ts
              remaining = t1 ++ tail t2
              BinomTree _ children = head t2

fromList :: (Heap h, Ord a) => [a] -> h a
fromList = foldr insert empty

toList :: (Heap h, Ord a) => h a -> [a]
toList h
    | isEmpty h = []
    | otherwise = (findMin h) : (toList $ deleteMin h)

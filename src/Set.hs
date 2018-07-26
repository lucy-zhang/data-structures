module Set where

import Data.List (foldl')

class Set s where
    empty :: s a
    insert :: Ord a => a -> s a -> s a
    member :: Ord a => a -> s a -> Bool

data SplayTree a =
    Empty
  | Tree (SplayTree a) a (SplayTree a)
  deriving Show

instance Set SplayTree where
    empty = Empty

    insert a = splay a . insert' a
        where insert' a' t = case t of
               Empty -> Tree Empty a' Empty
               Tree l x r
                    | a == x -> t
                    | a > x -> Tree l x (insert a' r)
                    | otherwise -> Tree (insert a' l) x r

    member = const $ const False

splay :: Ord a => a -> SplayTree a -> SplayTree a
splay _ Empty = Empty
splay a t@(Tree l x r)
    | a == x = t
    | a > x = case r of
        Empty -> t
        Tree l1 x1 r1
            | a == x1 -> Tree (Tree l x l1) x1 r1
            | a > x1 -> let t2 = splay a r1 in case t2 of
                Empty -> Tree (Tree l x l1) x1 Empty
                (Tree l2 x2 r2) -> Tree (Tree (Tree l x l1) x1 l2) x2 r2
            | otherwise -> let t2 = splay a l1 in case t2 of
                Empty -> Tree (Tree l x Empty) x1 r1
                (Tree l2 x2 r2) -> Tree (Tree l x l2) x2 (Tree r2 x1 r1)
    | otherwise = case l of
        Empty -> t
        Tree l1 x1 r1
            | a == x1 -> Tree l1 x1 (Tree r1 x r)
            | a < x1 -> let t2 = splay a l1 in case t2 of
                Empty -> Tree Empty x1 (Tree r1 x r)
                (Tree l2 x2 r2) -> Tree l2 x2 (Tree r2 x1 (Tree r1 x r))
            | otherwise -> let t2 = splay a r1 in case t2 of
                Empty -> Tree l1 x1 (Tree Empty x r)
                (Tree l2 x2 r2) -> Tree (Tree l1 x1 l2) x2 (Tree r2 x r)

lookup :: Ord a => a -> SplayTree a -> (Maybe a, SplayTree a)
lookup a t = (x, t')
    where
        t' = splay a t
        x = case t' of
            Empty -> Nothing
            Tree _ x' _ -> if x' == a then Just a else Nothing

fromList :: (Ord a, Set s) => [a] -> s a
fromList = foldl' (flip insert) empty

instance Set RBTree where
    empty = EmptyRB

    member _ EmptyRB = False
    member v (TreeRB _ v' left right)
        | v == v' = True
        | v < v' = member v left
        | otherwise = member v right

    insert v t = let
        ins EmptyRB = TreeRB R v EmptyRB EmptyRB 
        ins t'@(TreeRB c v' left right)
            | v == v' = t'
            | v < v' = balance $ TreeRB c v' (ins left) right
            | otherwise = balance $ TreeRB c v' left (ins right)
        TreeRB _ a left' right' = ins t
        in TreeRB B a left' right'

data Color
    = R
    | B
    deriving (Show, Eq)

data RBTree a
    = EmptyRB
    | TreeRB Color a (RBTree a) (RBTree a)
    deriving (Show, Eq)

balance :: Ord a => RBTree a -> RBTree a
balance (TreeRB B z (TreeRB R y (TreeRB R x a b) c) d) = TreeRB R y (TreeRB B x a b) (TreeRB B z c d)
balance (TreeRB B x a (TreeRB R y b (TreeRB R z c d))) = TreeRB R y (TreeRB B x a b) (TreeRB B z c d)
balance (TreeRB B z (TreeRB R x a (TreeRB R y b c)) d) = TreeRB R y (TreeRB B x a b) (TreeRB B z c d)
balance (TreeRB B x a (TreeRB R z (TreeRB R y b c) d)) = TreeRB R y (TreeRB B x a b) (TreeRB B z c d)
balance t = t

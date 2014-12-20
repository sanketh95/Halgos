module BTree(BTree, inorder, preoder, postorder) where

data BTree a = Empty | Branch a (BTree a) (BTree a)
               deriving (Show, Eq)

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Branch x lt rt) = (inorder lt) ++ [x] ++ (inorder rt)

preoder :: BTree a -> [a] 
preoder Empty = []
preoder (Branch x lt rt) = [x] ++ (preoder lt) ++ (preoder rt)

postorder :: BTree a -> [a]
postorder Empty = []
postorder (Branch x lt rt) = (postorder lt) ++ (postorder rt) ++ [x]



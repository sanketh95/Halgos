module BST(isBST) where
import BTree

type BST a = BTree a

isBST :: Ord a => BTree a -> Bool
isBST x = isSorted $ inorder x

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:(y:xs))
  | x > y = False
  | otherwise = isSorted (y:xs)

sort :: BST a -> [a]
sort x = inorder x
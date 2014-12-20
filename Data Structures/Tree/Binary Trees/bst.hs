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

sort :: Ord a => BST a -> [a]
sort x = inorder x

createFromSorted :: Ord a => [a] -> BST a
createFromSorted [] = Empty
createFromSorted (x:xs) = (Branch x (Empty) (createFromSorted xs))

createBalancedFromSorted :: Ord a => [a] -> BST a
createBalancedFromSorted = undefined

search :: Ord a => a -> BST a -> Bool
search _ Empty = False
search x (Branch y lt rt)
  | x == y = True
  | x < y = search x lt
  | x > y = search x rt
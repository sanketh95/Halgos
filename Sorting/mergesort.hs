-- Merge sort algorithm
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (fst ys)) (mergeSort (snd ys)) where ys = split xs

-- Split the array into two halves
split :: [Int] -> ([Int], [Int])
split xs = (take a xs, drop a xs) where a = (length xs) `div` 2

-- Merge two sorted arrays
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
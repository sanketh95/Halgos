-- Insertion Sort
-- By Sanketh Mopuru


insertionSort :: [Int] -> [Int]
insertionSort xs = insertionSort' [] xs

-- insertion sort algorithm
insertionSort' :: [Int] -> [Int] -> [Int]
insertionSort' xs [] = xs                                -- Entire list has been processed, so, return xs
insertionSort' [] (x:xs) = insertionSort' [x] xs         -- Processing the first element of the list
insertionSort' xs (y:ys)                                 
  | t > y = insertionSort' (insert xs y) ys              -- if last element of proc. list > first element of unproc. list
  | otherwise = insertionSort' (xs++[y]) xs              -- add y to processed list and process the remaining list
  where
    t = last xs

-- Insert an element into a sorted array
insert :: [Int] -> Int -> [Int]
insert [] x = [x]									     
insert [x] y = if x <= y then  [x,y] else [y,x]
insert (x:(y:xs)) z
  | x <= z && y >= z = [x,y,z] ++ xs
  | z < x = [z,x,y] ++ xs
  | otherwise = x : insert (y:xs) z
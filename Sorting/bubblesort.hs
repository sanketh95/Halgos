-- Bubble Sort
-- By Sanketh Mopuru

-- Bubble sort algorithm
bubbleSort :: [Int] -> [Int]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort a = bubbleSort (removeVal a m) ++ [m] where m = maxVal a

-- Maximum value in list
maxVal :: [Int] -> Int
maxVal [x] = x
maxVal (x:xs) = if x > m then x else m where m = maxVal xs

-- Remove the first occurence if a value in a list
removeVal :: [Int] -> Int -> [Int]
removeVal [] _ = []
removeVal (x:xs) v = if x==v then xs else x:removeVal xs v
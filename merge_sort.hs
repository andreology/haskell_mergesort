--A Haskell implementation of Quick and Merge Sort
--Andre Barajas, Spring 2020

--Creating a tester to verify quick sort and merge sort api's work
main :: IO () -- main method signiture required for haskell IO
main = do
      let output = msort[4, 65, 2, -31, 0, 99, 2, 83, 782, 1]
      print "Output of Merge Sort in Haskell"
      print output


--a Merge sort implementation using Haskell
merge :: Ord a => [a] -> [a] -> [a] --api signiture
merge [] [] = [] -- Consider base case of an empty list return 0
merge [] ys = ys --Merge of an empty list with a non empty list on left
merge xs [] = xs --Merge of an empty list with a non empty list on right
merge (x:xs) (y:ys) -- merging output from lists prev split
	| x <= y 	= x : merge xs (y:ys)
	| otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a] --api signiture
msort a
	| (length a) <= 1 	= a --deriving length with haskell length function
	| otherwise 		= merge (msort partOne) (msort partTwo)
		where
			half = (length a) `div` 2 -- dividing list with div and splitting list
			partOne = take half a
			partTwo = drop half a

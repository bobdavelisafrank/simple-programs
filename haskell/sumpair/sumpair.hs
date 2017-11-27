-- A solution to http://www.techiedelight.com/find-pair-with-given-sum-array/
import Data.List

{- Here I found that a pretty simple solution is just to sort the list, subtract
 - all the elements from 10, then find the intersections with the original list.
 -
 - If things have been kept sorted, then the resulting list have the pairs
 - encoded really simply into it, whereby every matching head and last of the
 - list makes the correct pair.
 -
 - This was done off of the realization that one can subtract each element
 - from the sum, then just check to see if any of the differences are in the
 - original list. From there, it expanded out from basic observation into this,
 - which is able to find all the pair-sums in the list.
 -}
sumpair :: (Num a, Ord a) => [a] -> a -> [(a, a)]
sumpair list sum =
  let
    sorted = sort list
    subbed = reverse $ map ((-) sum) sorted
    palind = subbed `intersect` sorted
  in
    takePairs palind []
  where
    -- There's a much better way of doing this here.
    takePairs :: [a] -> [(a, a)] -> [(a, a)]
    takePairs []  pairs = pairs
    takePairs [x] pairs = (x, x):pairs
    takePairs xs  pairs =
      takePairs newxs ((head xs, last xs):pairs)
        where
          intermediateXs = tail xs
          newxs = init intermediateXs


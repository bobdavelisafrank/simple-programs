-- file: agm.hs

{-
  Haskell code for generating the arithmetic-geometric mean of two numbers.

  Contains a small example at the end for approximating logarithms.
-}

-- Mutually recursive functions that both converge onto the arithmetic-geometric mean of a function.
amean :: (Integral a, Floating b) => a -> b -> b -> b
amean 1 x y = (x+y)/2
amean n x y = amean 1 (amean (n-1) x y) (gmean (n-1) x y)
gmean :: (Integral a, Floating b) => a -> b -> b -> b
gmean 1 x y = sqrt(x*y)
gmean n x y = gmean 1 (amean (n-1) x y) (gmean (n-1) x y)

-- Function which computes the arithmetic-geometric mean of two numbers, up to a degree of error epsilon.
agm :: (RealFloat a) => a -> a -> a -> a
agm x y epsilon = agmiter x y epsilon 1
  where agmiter x y epsilon n =
          let (a,b) = iterated n x y
          in  if abs (a-b) <= epsilon
                then a
                else agmiter x y epsilon (n+1)
        iterated :: (Integral a, Floating b) => a -> b -> b -> (b,b)
        iterated n x y = (amean n x y, gmean n x y)

-- Function which computes the arithmetic-geometric mean of two numbers, with a minimum accuracy of 10^(-14).
agMean :: (RealFloat a) => a -> a -> a
agMean x y = agm x y (10**(-14))

-- An example of the use of the arithmetic mean: computing logarithms.
-- The log of 2 can be approximated using a more intensive calculation, but other logarithms can be calculated quite easily now.
logarithm :: (RealFloat a) => a -> a
logarithm x = 
  if x > 0
    then ln x 32
    else 0/0 -- Naive way to throw NaN
  where ln x m = acos0/(2*(agMean 1 (4/s))) - m*(log 2) where s = x*(2**m)
        acos0 = 3.14159265358979323846
        ln2 = 0.69314718055994530942

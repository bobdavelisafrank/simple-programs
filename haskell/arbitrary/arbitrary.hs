-- A bit of messing around with implementing arbitrary-precision arithmetic.
--
-- It's not a very effecient implementation, nor a very effect nor complete one,
-- but it was a bit of fun to work out.
import Data.Char (intToDigit)



-- Base for the calculation.
base :: (Integral a) => a
base = 10



{- For retrieving digits from actual numeric values:
 -
 - lastDecimal ->
 -   Gets the last digit of the given number.
 -
 - initDecimal ->
 -   Returns the given number with all but the last digit.
 -}
lastDecimal :: (Integral a) => a -> a
lastDecimal n = n `mod` base

initDecimal :: (Integral a) => a -> a
initDecimal n = (n - (lastDecimal n)) `div` base



-- Converts a number into a list of digits, for arbitrary-precision arithmetic.
listFromIntegral :: (Integral a) => a -> [a]
listFromIntegral x = 
  if x < 0 
     then listNegate . listFromIntegralIter (negate x) $ []
     else listFromIntegralIter x []
  where
    listFromIntegralIter :: (Integral a) => a -> [a] -> [a]
    listFromIntegralIter x digits
      | x >= base  =
          listFromIntegralIter (initDecimal x) ((lastDecimal x):digits)
      | otherwise = (x:digits)



-- Converts a list of digits to a string. Not a working solution for large
-- bases.
stringOfList :: [Int] -> String
stringOfList (x:xs) = 
  if x < 0
     then (:) '-' . map intToDigit . listNegate $ (x:xs)
     else map intToDigit (x:xs)



-- Negates a list-number by negating each digit.
listNegate :: (Integral a) => [a] -> [a]
listNegate = map (negate)



-- Removes the heading zeros from a list-number.
removeHeadingZeros :: (Integral a) => [a] -> [a]
removeHeadingZeros [] = []
removeHeadingZeros (x:xs) = 
  if x == 0
    then removeHeadingZeros xs
    else (x:xs)



-- Takes two list-numbers and adds them.
numeralListAdd :: (Integral a) => [a] -> [a] -> [a]
numeralListAdd x y = numeralListIter (reverse x) (reverse y) [] 0
  where
    numeralListIter :: (Integral a) => [a] -> [a] -> [a] -> a -> [a]
    numeralListIter [] ys as 0 = (reverse ys) ++ as
    numeralListIter xs [] as 0 = (reverse xs) ++ as
    numeralListIter [] ys as c = numeralListIter [c] ys as 0
    numeralListIter xs [] as c = numeralListIter xs [c] as 0
    numeralListIter (x:xs) (y:ys) as c =
      numeralListIter xs ys
      ((lastDecimal (x + y + c)):as) (initDecimal (x + y + c))



-- Takes two numbers written as lists of digits and subtracts them.
numeralListSubtract :: (Integral a) => [a] -> [a] -> [a]
numeralListSubtract x y = 
  -- Adds x to y*(-1) in order to subtract.
  let result = numeralListAdd x (listNegate y)
  
  -- If there is a negative digit in the result, then it has to balance the
  -- number using a special function. If not, then it just removes heading
  -- zeros (if any).
  in if negativeDigitIn result
     then removeHeadingZeros . negativeBalance $ result
     else removeHeadingZeros result
  where
    -- Checks a list of digits to see if one of the digits is negative.
    negativeDigitIn :: (Integral a) => [a] -> Bool
    negativeDigitIn [] = False
    negativeDigitIn (x:xs) =
      if x < 0
      then True
      else negativeDigitIn xs
      
    -- Removes positive digits from negative numbers by swapping the sign on
    -- the number, removing the negative digits, and then swapping the sign on
    -- the result again.
    negativeBalance :: (Integral a) => [a] -> [a]
    negativeBalance x =
      listNegate
      (unbase
       (numeralListIter
        (reverse . listNegate $ x)
        (replicate (length x) base)
        []
        0
       ))
      where
        lastDecimalBalancer :: (Integral a) => a -> a
        lastDecimalBalancer n =
          if n == base
          then n
          else lastDecimal n
        initDecimalBalancer n = 
          if n == base
          then 0
          else initDecimal n
          
        numeralListIter :: (Integral a) => [a] -> [a] -> [a] -> a -> [a]
        numeralListIter [] ys as 0 = (reverse ys) ++ as
        numeralListIter xs [] as 0 = (reverse xs) ++ as
        numeralListIter [] ys as c = numeralListIter [c] ys as 0
        numeralListIter xs [] as c = numeralListIter xs [c] as 0
        numeralListIter (x:xs) (y:ys) as c =
          numeralListIter
            xs
            ys
            ((lastDecimalBalancer (x + y + c)):as)
            (initDecimalBalancer (x + y + c))
          
    unbase :: (Integral a) => [a] -> [a]
    unbase x = reverse . unbaser x $ []
      where
        unbaser :: (Integral a) => [a] -> [a] -> [a]
        unbaser [] as = as
        unbaser [x] as = (x:as)
        unbaser (x:xs) as = unbaser xs ((x-1):as)
    {- Non-uniform negative digits are removed using a little arithmetic trick.
     - A base 10 example:
     -
     -         28 = 30 - 2 = [3,-2]
     -            = [3,-2] + [10,10] - [10,10]
     -            = [13,8] - [10,10]
     -            = [1,3,8] - [1,1,0] <- Digit carry
     -            = [0,2,8]
     -            = 28
     - 
     - We add 10 to each digit to make it positive, rewrite the number as a
     - valid base 10 number, and subtract the added amount (rewritten as a
     - valid base 10 number) without any carrying.
     -
     - Really simple, right? The issue is that it doesn't actually work 100% of
     - the time.
     -
     -         91 = 100 - 9 = [1,0,-9]
     -            = [1,0,-9] + [10,10,10] - [10,10,10]
     -            = [11,10,1] - [10,10,10]
     -            = [1,2,0,1] - [1,1,1,0]
     -            = [0,1,-1,1]
     -
     - The fix is that if the digit is exactly equal to 10 in the number, you
     - don't carry it (exluding the added amount).
     -
     -            ...
     -            = [11,10,1] - [10,10,10]
     -            = [1,1,10,1] - [1,1,1,0]
     -            = [0,0,9,1]
     -            = 91
     -
     - The reason why this implementation is so large is because we just have
     - to entirely reimplement the addition function with new carry functions
     - as well as adding in the additional logic.
     -}



-- Multiplies two list-integers.
karatsuba :: (Integral a) => [a] -> [a] -> [a]
karatsuba [] [] = []
karatsuba [x] [y] = numeralListAdd [x*y] [0]
karatsuba (x:xs) (y:ys)
  | xlen == ylen =
    let
      z2 = karatsuba [x] [y]
      z0 = karatsuba xs ys
      z1 =
        ((karatsuba
           (numeralListAdd [x] xs)
           (numeralListAdd [y] ys)
          ) `numeralListSubtract` z2
        ) `numeralListSubtract` z0
      b  = replicate (length xs) 0
      b2 = replicate (2*length xs) 0
    in removeHeadingZeros
    ((numeralListAdd (z2 ++ b2) (z1 ++ b)) `numeralListAdd` z0)
  | xlen > ylen = karatsuba (x:xs) (padZeros (xlen - ylen) (y:ys))
  | otherwise   = karatsuba (padZeros (ylen - xlen) (y:ys)) (x:xs)
  where
    xlen = length xs
    ylen = length ys
    
    padZeros :: (Integral a) => Int -> [a] -> [a]
    padZeros 0 xs = xs
    padZeros n xs = padZeros (n-1) (0:xs)

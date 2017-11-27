-- file: sieve.hs

-- Needed for command-line arguments.
import System.Environment (getArgs)

{- A prime-generator which generates primes using position instead of by
 - testing divisibility.
 -}

-- Parses the args.
main :: IO ()
main = do
  args <- getArgs

  if (length args) < 1
  then putStrLn "Error: Not enough arguments."
  else writePrimes (read $ head args)


-- Prints primes to the file.
writePrimes :: (Integral a, Show a) => a -> IO ()
writePrimes upperBound = 
  writeFile "sieve.txt"
    . mconcat
    . map (\x -> (show x) ++ "\n")
    . sieve
    $ upperBound


-- Generates a list of prime numbers by removing a list of composite numbers.
sieve :: (Integral a) => a -> [a]
sieve n =
  let startingPrimes = primesLessThan n
      upperBound     = (last startingPrimes) ^ 2
  in if n > 0
     then (++) startingPrimes . tail $ removeMultiples startingPrimes [1..upperBound]
     else []
  where -- For every number N in the input list, it removes every 'n'th element from a list.
        removeMultiples :: (Integral a) => [a] -> [a] -> [a]
        removeMultiples [] ys     = filter (>0) ys
        removeMultiples (x:xs) ys = removeMultiples xs (replaceMultiple x ys)
        
        -- Replaces every number in a position divisible by 'n' with a negative number 
        -- (for filtering). Basically, removes every 'n'th element.
        replaceMultiple :: (Integral a) => a -> [a] -> [a]
        replaceMultiple n list = replaceIter n list (1-n)
          where replaceIter :: (Integral a) => a -> [a] -> a -> [a]
                replaceIter _ [] _     = []
                replaceIter n (x:xs) 0 = (-1):(replaceIter n xs (1))
                replaceIter n (x:xs) y = x:(replaceIter n xs (mod (y+1) n))

-- A wholistic prime generator, to use to generate a small number of starting
-- primes with. Not a sieve, but very helpful for one.
generatePrimes :: (Integral a) => a -> [a]
generatePrimes numberOfPrimes
  | numberOfPrimes <= 0 = []
  | numberOfPrimes == 1 = [2]
  | numberOfPrimes == 2 = [2, 3]
  | otherwise           = divisorExpander (numberOfPrimes-2) [2, 3]

-- Function which gets a list of the primes less than a number (overestimates as
-- necessary).
primesLessThan :: (Integral a) => a -> [a]
primesLessThan =
  generatePrimes
  . ceiling
  . (*) 1.25506
  . pApprox
  where
    pApprox n =
      let n' = fromIntegral n
      in  n' / (log n')
  
{- Function that, when given an integer X and a list of integers, adds X
 - new integers for which no element in the list divides into.
 -}
divisorExpander :: (Integral a) => a -> [a] -> [a]
divisorExpander _ [] = []
divisorExpander amountToExpand divisors = 
  coprimeListExpander amountToExpand 1 (last divisors) divisors
    
{- Tail recursive helper-function for the function directly above.
 - Note: The "canidate" (under the 'where' statement) should just be
 -       (index + initialListHead), but the index is doubled
 -       in order to only test odd numbers to aid prime generation.
 - Note: The layout of this function is why the output list of primes
 -       is backwards.
 - Note: Any primes less than the square-root of the number are filtered
 -       out in advance.
 -}
coprimeListExpander :: (Integral a) => a -> a -> a -> [a] -> [a]
coprimeListExpander amountToExpand index initialListHead coprimeList
  | amountToExpand < 1 = coprimeList
  | coprimeTest (canidate) $ takeWhile (<= squareRoot canidate) coprimeList =
      coprimeListExpander (amountToExpand - 1) (index + 1) initialListHead (coprimeList ++ [canidate])
  | otherwise =
      coprimeListExpander amountToExpand (index + 1) initialListHead coprimeList
  where
    canidate = 2 * index + initialListHead

-- Function for testing if a number is coprime to every number in a list.
coprimeTest :: (Integral a) => a -> [a] -> Bool
coprimeTest canidate = and . map (\x -> canidate `mod` x /= 0)

-- Integer square-root function taken from the Haskell Wiki.
squareRoot :: (Integral a) => a -> a
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
  let twopows             = iterate (^!2) 2
      (lowerRoot, lowerN) = last $ takeWhile ((n >=) . snd) $ zip (1:twopows) twopows
      newtonStep x        = div (x + div n x) 2
      iters               = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
      isRoot r            = r^!2 <= n && n < (r+1)^!2
  in head $ dropWhile (not. isRoot) iters
  where
    (^!) :: Num a => a -> Int -> a
    (^!) x n = x^n

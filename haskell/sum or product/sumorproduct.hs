-- file: sumorproduct.hs

-- CLI program for which the user may ask for a sum or product over a range
-- of numbers starting at 1.

-- Module needed for interpreting text.
import Data.Char (toLower)
-- Module for dealing with output text.
import System.IO



-- Function that gives you either a sum or product of the list of numbers 1 to
-- x, based on the given character.
productOrSum :: (Integral a) => Char -> a -> a
productOrSum functionName x
  | functionName == 'p' = product [1..x]
  | functionName == 's' = sum [1..x]



main = do
  -- Asks user if they want a sum or a product.
  putStr "Sum or product? "
  hFlush stdout
  functionType <- getLine

  -- Asks user from what numbers to sum or take the product of.
  putStr "From 1 to? "
  hFlush stdout
  number <- getLine

  -- I decided to implement lazy auto-correction, like in GNU parted.
  let validSpellings =
        (map (flip take "product") [1..(length "product")]) ++
        (map (flip take "sum") [1..(length "sum")])

  -- If the spelling of 'sum' or 'product' is sufficient, then it prints the
  -- resulting sum or product.
  -- If it is not, then it complains, and restarts the program.
  if (map toLower functionType) `elem` validSpellings
    then putStrLn . show $
         productOrSum (toLower $ head functionType) (read number)
    else do
      putStrLn "Sorry, I wasn't programmed to handle that option."
      main

-- file: triangle.hs

-- Needed for commandline arguments.
import System.Environment (getArgs)

-- Main executable code block. Prints 'n' triangle squares, where 'n' is the
-- only commandline argument.
main = do
  n <- getArgs
  mapM_ print . map triangle $ [0..((read . head $ n) - 1)]
  
-- Function for finding the nth triangle square.
triangle :: (Integral a) => a -> a
triangle n = triangleIter 1 0 n
  where triangleIter :: (Integral a) => a -> a -> a -> a
        triangleIter a b 0 = a
        triangleIter a b n = triangleIter (34*a-b+2) a (n-1)

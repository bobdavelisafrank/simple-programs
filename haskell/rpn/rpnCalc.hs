{- Reverse polish notation calculator.
 -
 - Written while reading `Learn You A Haskell` chapter 10, though this was an
 - attempt made previous to the implementation made in the chapter.
 -}

import Data.Char (toLower, isNumber)
import Data.Fixed (mod')
import System.IO (isEOF)

{- Takes a reverse-polish notation expression, and evaluates it.
 -
 - It tokenizes the input, and applies that input to an infinite 0-initialized
 - stack. It then returns the head of this stack.
 -
 - Since no error-propogation friendly types are currently in use in this
 - prototype, underflows are invisible (the primary issue of this calculator).
 -}
rpnParser :: String -> Double
rpnParser expression = rpnIter (words expression) (repeat 0)
  where {- Evaluates the stack, based on tokenized string input.
         -
         - When the input runs out, it returns the head of the stack (since it
         - currently cannot perform I/O actions).
         -
         - It also currently assumes that there are at least 2 things in the
         - stack when it starts. Not very safe. This can also be solved with
         - the use of error propogating types.
         -}
        rpnIter :: [String] -> [Double] -> Double
        rpnIter [] stack = head stack
        rpnIter (p:parts) (stack1 : stack2 : stack)
          -- List of all stack commands.
          | isDoubleString p = iter [(read p :: Double), stack1, stack2]
          | is "dup"  = iter [stack1, stack1]
          | is "flip" = iter [stack2, stack1]
          | is "+"    = iter [stack2 +  stack1]
          | is "-"    = iter [stack2 -  stack1]
          | is "*"    = iter [stack2 *  stack1]
          | is "/"    = iter [stack2 /  stack1]
          | is "**"   = iter [stack2 ** stack1]
          | is "^"    = iter [stack2 ** stack1]
          | is "mod"  = iter [mod' stack2 stack1]
          | is "%"    = iter [mod' stack2 stack1]
          | is "pi"   = iter [pi, stack1, stack2]
          | is "e"    = iter [(exp 1), stack1, stack2]
          | is "neg"  = oneArg negate
          | is "sqrt" = oneArg sqrt
          | is "exp"  = oneArg exp
          | is "log"  = oneArg log
          | is "sin"  = oneArg sin
          | is "cos"  = oneArg cos
          | is "tan"  = oneArg tan
          | is "asin" = oneArg asin
          | is "acos" = oneArg acos
          | is "atan" = oneArg atan
          | otherwise = iter [stack1, stack2]
          where {- Stack command helper functions:
                 -
                 - iter   -> Replaces the topmost two values on the stack with
                 -           the values given in a list.
                 - is     -> Shortcut for "p ==", since it occurs so often.
                 - oneArg -> Applies a one-argument function to the value at
                 -           the top of the stack.
                 -
                 - isDoubleString -> For telling when numbers are being pushed
                 -                   onto the stack.
                 -}
                iter val = rpnIter parts (val ++ stack)
                is       = (==) p
                oneArg f = iter (f stack1 : stack2 : [])
 
                isDoubleString :: String -> Bool
                isDoubleString string = isNumberString . clean $ string
                  where -- Removes the first occurance of a symbol in a string.
                        removeOneSym :: Char -> String -> String
                        removeOneSym _ [] = []
                        removeOneSym sym (x : xs) =
                          if x == sym
                          then xs
                          else x : (removeOneSym sym xs)

                        -- Removes the non-numeric symbols of a float string.
                        clean = removeOneSign . removeOnePeriod
                          where removeOneSign   = removeOneSym '-'
                                removeOnePeriod = removeOneSym '.'

                        -- Tells if a string consists of only numeric chars.
                        isNumberString :: String -> Bool
                        isNumberString []     = False
                        isNumberString string = and . map isNumber $ string

main = do
  -- Detects EOF, in order to react gracefully.
  end <- isEOF
  if end
    then return ()
    else do
      -- Gets the input to parse (in lower-case, for consistent parsing).
      expression <- fmap (map toLower) getLine

      {- Detects if the input is an 'exit' command; if not, it parses the input,
       - and loops. Otherwise, it quits.
       -}
      if or (map ((==) expression) ["quit", "exit", "q", "bye"])
        then return ()
        else if or (map ((==) expression) ["help", "h"])
               then do
                 mapM_ putStrLn
                 ["Reverse-polish-notation calculator.",
                   "",
                   "    quit -> Exit the calculator.",
                   "            Aliases: q, exit, bye",
                   "    help -> Shows this menu.",
                   "            Aliases: h",
                   "",
                   "Arithmetic Operations: ",
                   "    x y +  -> Adds two numbers.",
                   "    x y -  -> Subtracts y from x.",
                   "    x y *  -> Multiplies two numbers.",
                   "    x y /  -> Divides y from x.",
                   "    x y ** -> Takes x to the power of y.",
                   "              Aliases: ^",
                   "    x y %  -> Takes x reduction-modulo y.",
                   "              Aliases: mod",
                   "    x neg  -> Negates x.",
                   "    x sqrt -> Takes the square-root of x.",
                   "",
                   "Transcendental Functions: ",
                   "    pi     -> Puts pi (the constant) onto the stack.",
                   "    e      -> Puts Euler's Number onto the stack.",
                   "    x exp  -> exp(x), or e to the power of x.",
                   "    x log  -> Takes the natural log of x.",
                   "    x sin  -> Takes the sine of x.",
                   "    x cos  -> Takes the cosine of x.",
                   "    x tan  -> Takes the tangent of x.",
                   "    x asin -> Takes the arc-sine of x.",
                   "    x acos -> Takes the arc-cosine of x.",
                   "    x atan -> Takes the arc-tangent of x.",
                   "",
                   "Stack Operations: ",
                   "    dup    -> Duplicates the number on top of the stack.",
                   "    flip   -> Flips the top two numbers on the stack.",
                   ""]
                   
                   main
               else do
        print $ rpnParser expression
          main
  

-- file: guess.hs

-- Needed for some output handling.
import System.IO



-- Function that tells if the guess is correct, by comparison.
correct :: Integer -> Ordering
correct newGuess = compare newGuess secretNumber
  where
    secretNumber = 42



{- Function that handles the answer. It tells whether the guessed number was
 - correct,
 - less than, or equal to the secret number. On a correct guess it also prints
 - a list of all the guesses.
 -} 
reply :: Ordering -> [Integer] -> IO ()
reply GT _          = putStrLn "The guess was too large."
reply LT _          = putStrLn "The guess was too small."
reply EQ guesses = do
  putStrLn "The guess was correct!"
  putStr "Guesses: "
  print $ reverse guesses

  

-- Game loop. Requests a number, and tells its accuracy.
gameIter :: Ordering -> [Integer] -> IO ()
gameIter EQ guessesList = putStr ""
gameIter _  guessesList = do
  putStr "Guess the number: "
  hFlush stdout
  newGuess <- getLine

  let guessCompared = correct . read $ newGuess
  let newGuessesList = (read newGuess):guessesList

  reply guessCompared newGuessesList
  
  gameIter guessCompared newGuessesList

  

-- Executed code: Runs the guessing game.
main = gameIter LT []

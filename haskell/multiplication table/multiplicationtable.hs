-- file: multiplicationtable.hs

-- Module needed for list manipulation (for the printing of the tables).
import Data.List



-- Function that creates a multiplication table with X columns and X rows.
createMultiplicationTable :: Int -> [[Int]]
createMultiplicationTable x =
  zipWith (\x y -> map (x*) y) [1..] $ replicate x [1..x]



-- Function that takes a created table and prints it.
printTable table = mapM_ putStrLn . lines $ flattenTable table
  where 
    padSpaces :: Int -> String -> String
    padSpaces x string = 
      if length string < x
      then padSpaces x (' ':string)
      else string

    -- Crunch down the table into printable text.
    flattenTable table =
      (concat $
       concat $
       intersperse ["\n"] $
       map (map (padSpaces 4 . show)) table
      ) ++ "\n"



main = printTable $ createMultiplicationTable 12

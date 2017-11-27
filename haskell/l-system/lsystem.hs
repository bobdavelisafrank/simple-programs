-- file: lsystem.hs

-- A small implementation of L-systems in Haskell.

module Lsystems
( transform,
  nTransform,
  rules,
  axiom,
  applyRuleSet
) where

{-
  Basic interface:
  
  Rules are a list of tuples, storing ('A', "AB") for A -> AB.
  Notice that the first is a character (apostrophe quotations), and the second
  a string (normal quotations).

  An axiom is stored as just a string, as are any transformations.

  'transform' takes a ruleset and a system, then applies the rules to a system.

  'nTransform' takes a number N, a ruleset, and a system, then applies N
  transforms to a system with that ruleset.

  'rules' is just an example rule list, as is 'axiom'.

  'applyRuleSet' takes a set of rules and an element of a system, then applies
  the appropriate rule to that element.
-}

-- Rules, evaluated head-first.
rules = [('A',"AECA"),('C',"CEA")]



-- Axiom. Starting string, to which transformations are applied.
axiom :: String
axiom = "A"



-- Transform. Makes a transformation of an L-system by applying the rules to
-- each element of the system.
transform :: [(Char,String)] -> String -> String
transform _ [] = []
transform ruleSet (element:system) =
  (applyRuleSet ruleSet element) ++ (transform ruleSet system)



-- nTransform. Applies n transformations to a system.
nTransform :: (Integral a) => a -> [(Char,String)] -> String -> String
nTransform n ruleSet system
  | n <= 0    = system
  | otherwise = nTransform (n-1) ruleSet (transform ruleSet system)



-- Function that finds a matching rule from a list of rules, then applies it
-- to an element.
applyRuleSet :: [(Char,String)] -> Char -> String
applyRuleSet [] char = [char]
applyRuleSet ((match,replacement):ruleSet) char = 
  if char == match 
    then replacement 
    else applyRuleSet ruleSet char

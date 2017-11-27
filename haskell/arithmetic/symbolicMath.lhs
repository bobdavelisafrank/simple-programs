file: symbolicMath.lhs

This is a first-draft first-attempt prototype for myself of trying to implement 
symbolic computation. For some reason, I'm trying to implement it from scratch
instead of just researching it.

A few of the starting ideas:

As evident from languages like Lisp, any function can be written as a tree of
operations and arguments.

  Psuedo-lisp:
    (* a 
       (+ b c))

  Diagram:
        (*)
        / \
       /   \
     (+)    a
     / \
    b   c

With such trees, we can start to perform transformations (as per the algebra).

  PL:
    (+ (* a b)
       (* a c))

  D:
        (+)
        / \
       /   \
     (*)   (*)
     / \   / \
    a   b a   c

This gives us ideas on how to store, manipulate, and evaluate functions. Playing
with these trees a bit shows that they have a fairly simple grammar (Note: not
a linguist): each node is a part of the function, and each part of the function
consists of either an operation on other parts, a number, or a variable. 

> data FuncPart = Op String [FuncPart] | Val Integer | Var String deriving (Show)

For internal representation, I'm storing operation and variable names as 
strings, allowing for arbitrary operations and variable names.

The numbers are currently to be stored as integers, giving us exact precision if
we leave certain trees

  PL:
    (/ 2 3)

  D:
     (/)
     / \
    2   3

as they stand. Approximate answers may then be calculated if necessary.

A few examples of what these functions look like, using this tree system:

A simple function: f(x) = x + 2.

> exampleFunction1 =
>   (Op "add" [Val 2, Var "x"])

A slightly more complex function: f(x) = 2x^2 + 3x.

> exampleFunction2 =
>   (Op "add" [Op "mul" [Op "mul" [Var "x",
>                                  Var "x"],
>                        Val 2],
>              Op "mul" [Var "x",
>                        Val 3]])

(Note the stacked multipliers. This is not a limitation of the FuncPart type,
and is instead due to operation handling, for now.)

The big flexibility of this type is that it allows us to create the function

  f(x) = exampleFunction1(x) + sqrt(exampleFunction2(x))

with minimal adaptation:

> exampleFunction3 =
>   (Op "add" [exampleFunction1,
>              Op "sqrt" [exampleFunction2, Val 0]])

The simplest algebraic method we can implement is that of 'plugging in' values
for variables:

> substituteVar :: String -> FuncPart -> FuncPart -> FuncPart
> substituteVar  _ _ (Val value) = 
>   (Val value)
> substituteVar varName value (Var name) = 
>   if name == varName
>     then value
>     else (Var name)
> substituteVar varName value (Op opName argList) =
>   (Op opName 
>       (map (substituteVar varName value)
>            argList))

Note the type signature. We actually replace variables with other 
function-parts, instead of just integers. This means that a function such as

  f(x,y) = exampleFunction1(x) + exampleFunction2(y)

can also be expressed:

> exampleFunction4 =
>   (Op "add" [exampleFunction1,
>              substituteVar "x" (Var "y") exampleFunction2])

This also gives us a starting point for evaluating the functions.

  ***

The above method of mapping out functions calls for a way to get an operation 
based on its name, and a way of telling which operations we cannot evaluate 
(if we're expecting an exact answer).

For now, I'll leave these operations in one big list of working functions, and
a second big list of non-working functions.

> type OpReg    = [(String, (Integer -> Integer -> Integer))]
> type FlopReg  = [String]

Due to the immutability of vars, I'll be making the functions use a packaged-up
register type.

> type Register = (OpReg, FlopReg)

A more complete register will likely be needed. I'd like the register to end up
having a type signature more like:
  
  type Register = [(String, 
                    Maybe ([Integer] -> Integer), 
                    Maybe ([RealFloat] -> RealFloat))]

And, perhaps, later inclusions of operator properties (like associativity).

The main reason is that the current register discourages anything that doesn't
have exactly 2 arguments, despite the FuncPart type being made to support any
amount of arguments. The second reason is that there isn't a neat way to package
up the approximation functions for calculating approximate answers, and this
would provide that. The third reason is that we need 'maybe' ops for when we
want to leave the definition of a function undefined (for either integers or
floats).

This will certainly be made up in the next revision.

Until then, a default register of operations has been provided.

> opReg1 :: OpReg
> opReg1 = [("add", ( + )),
>           ("sub", ( - )),
>           ("mul", ( * )),
>           ("pow", (\x y -> x ^ y))]
>
> flopReg1 :: FlopReg
> flopReg1 = ["div",
>             "sqrt",
>             "log",
>             "exp", -- e^x
>             "sin",
>             "cos",
>             "tan",
>             "arcsin", "asin",
>             "arccos", "acos",
>             "arctan", "atan"]
>
> reg1 :: Register
> reg1 = (opReg1, flopReg1)

And, with it, a way of searching through and finding the functions that the
operations map to.

> findOp :: String -> OpReg -> (Bool, (Integer -> Integer -> Integer))
> findOp _ [] = (False, (\x y -> x))
> findOp opName ((name, op):reg) =
>   if name == opName
>     then (True, op)
>     else findOp opName reg
>
> findFlop :: String -> FlopReg -> Bool
> findFlop _ [] = False
> findFlop opName (name:reg) =
>   if name == opName
>     then True
>     else findFlop opName reg
>
> getOp :: String -> Register -> (Bool, (Integer -> Integer -> Integer))
> getOp name (opReg, flopReg) =
>   if (findFlop name flopReg)
>     then (False, (\x y -> x))
>     else (findOp name opReg)

With those, we can then search the registers for the operations, and apply them
to the functions in order to do basic arithmetic.

> applyReg :: Register -> FuncPart -> FuncPart
> applyReg reg (Op opName [Val arg1, Val arg2]) =
>   let (opFound, maybeOp) = getOp opName reg
>   in  if (opFound)
>         then (Val (maybeOp arg1 arg2))
>         else (Op opName [Val arg1, Val arg2])
> applyReg reg (Op opName argList) =
>   if (hasVars (Op opName argList)) ||
>      (hasFlops reg (Op opName argList))
>     then (Op opName (map (applyReg reg) argList))
>     else applyReg reg (Op opName (map (applyReg reg) argList)) -- Self applies as many times as needed.
> applyReg _ funcPart = funcPart
> 
> hasVars :: FuncPart -> Bool
> hasVars (Var _) = True
> hasVars (Val _) = False
> hasVars (Op _ argList) = or . map hasVars $ argList
>
> hasFlops :: Register -> FuncPart -> Bool
> hasFlops reg (Op opName argList) =
>   if findFlop opName (snd reg)
>     then True
>     else or . map (hasFlops reg) $ argList
> hasFlops _ _ = False

'applyReg', with a single application, will go through the function and perform
all the basic arithmetic it has from the provided registry (the use of hasFlops
and hasVars lets it know when it can reduce further).

Combining variable substitution with 'applyReg' allows us to evaluate functions
at specific values. A simple procedure allows us to take a register, a function,
and a list of variable substitutions and produce an evaluated function.

> eval :: Register -> FuncPart -> [(String, Integer)] -> FuncPart
> eval reg function [] = applyReg reg function
> eval reg function ((varName, value):vars) =
>   (eval reg
>         (substituteVar varName (Val value) function)
>         vars)

Actual algebraic rules and simplifications are yet to be implemented, but from
here it should be as simple as analyzing them and coding them in.

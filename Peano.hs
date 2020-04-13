module Haskell.Codewars.Peano where
import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano

-- Addition
add Zero x = x
add x Zero = x
add (Succ x) (Succ y) = (Succ (Succ (add x y)))

-- Subtraction
sub x Zero = x
sub Zero _ = error "negative number"
sub (Succ x) (Succ y) = sub x y

-- Multiplication
mul Zero _ = Zero
mul _ Zero = Zero
mul (Succ Zero) x = x
mul x (Succ Zero) = x
mul x (Succ y) = add x (mul x y)

even, odd :: Peano -> Bool

-- Even
even Zero = True
even (Succ Zero) = False
even (Succ (Succ x)) = even x

-- Odd
odd = not . even

compare :: Peano -> Peano -> Ordering
-- Compare
compare Zero Zero = EQ
compare Zero _ = LT
compare _ Zero = GT
compare (Succ x) (Succ y) = compare x y

-- Integer division
div _ Zero = error "divide by 0"
div Zero _ = Zero
div x (Succ Zero) = x
div x y | comp == EQ = (Succ Zero)
        | comp == LT = Zero
        | otherwise =  (Succ (div (sub x  y) y))
        where comp = (compare x y)

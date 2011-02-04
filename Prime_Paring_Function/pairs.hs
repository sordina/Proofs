import Control.Monad.Logic
import Data.Maybe
import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- This program demonstrates a mapping between the pairs of natural numbers and a (nonstrict?) subset of co-primes.
-- The property should hold for all sized lists, not just pairs. Ordering is preserved.
-- Question: Are we able to compress the range to create a bijection?
-- Answer: Yes! We can use the breadth-wise indces of the products, rather than the products themselves.

-- Pair to Number

x_p = (evens !!)
y_p = (odds  !!)

xy_p x y = x_p x * y_p y

xy_c x y = ns_nc $ xy_p x y

-- Number to Pair

p_xy n = (x,y)
  where
    x = fromJust $ findIndex (`divides` n) evens
    y = fromJust $ findIndex (`divides` n) odds

c_xy = p_xy . nc_ns

-- Sparse Number to Compact Number

numbers = odds >>- \x -> evens >>- \y -> return (x*y) -- I don't really understand LogicT...

ns_nc n = fromJust $ findIndex (==n) numbers

-- Compact Numner to Sparse Number

nc_ns = (numbers !!)

-- Helpers

evens = map (primes !!) [0,2..]
odds  = map (primes !!) [1,3..]

x `divides` y = y `mod` x == 0

-- Id referencing properties

prop_p1 = forAll (elements [(x,y) | x <- [1..6], y <- [1..7]]) f
  where
    f (x,y) = (x,y) == c_xy (xy_c x y)

prop_p2 = forAll (elements [1..100]) f
  where
    f c = c == uncurry xy_c (c_xy c)

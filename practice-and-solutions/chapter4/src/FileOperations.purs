module FileOperations where

import Prelude

import Data.Path (Path, ls)
import Data.Array -- (concatMap, (:), null, filter)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- Recursion exercises
----------------------
-- 1.
isEven :: Int -> Boolean
isEven i =
  case (i-2) of
    0 -> true
    1 -> false
    _ -> isEven (i-2)

-- 2.
-- Works, but it takes a LOT of time. QUESTION: Make it more efficient.
sumEven :: Array Int -> Int
sumEven arr =
  if null arr
    then 0
    else (go arr) + (sumEven $ unsafePartial tail arr)
      where
        go arr =
          case isEven $ unsafePartial head arr of
            true -> 1
            false -> 0

-- Filter/map exercises
-----------------------
--
-- QUESTION: Make 1. and 2. more general as currently they
--           would only work with `Int`s.
--
-- 1. (Easy) Use  the `map` or  `<$>` function to  write a
--    function which calculates the squares of an array of
--    numbers.
squares :: Array Int -> Array Int
squares as = map (\e -> e * e) as

-- 2. (Easy) Use the `filter` function to write a function
--    which removes the negative  numbers from an array of
--    numbers.
removeNegatives :: Array Int -> Array Int
removeNegatives = filter (\e -> e >= 0)

-- 3. (Medium)   Define  an   infix  synonym   `<$?>`  for
--    `filter`.  Rewrite  your   answer  to  the  previous
--    question to  use your new operator.  Experiment with
--    the  precedence  level  and  associativity  of  your
--    operator in PSCi.
infix 8 filter as <$?>
removeNegatives' :: Array Int -> Array Int
removeNegatives' as = (\e -> e >= 0) <$?> as

-- Guard exercises
------------------
--
-- 1. (Easy)  Use  the  `factors`  function  to  define  a
--    function  `isPrime`  which   tests  if  its  integer
--    argument is prime or not.
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i,j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

-- 2. (Medium) Write a function  which uses do notation to
--    find the _cartesian product_ of two arrays, i.e. the
--    set of all pairs of  elements `a`, `b`, where `a` is
--    an element of the first array, and `b` is an element
--    of the second.
cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct as bs = do
  i <- as
  j <- bs
  pure [i,j]

-- 3. (Medium)  A  _Pythagorean  triple_ is  an  array  of
--    numbers `[a,  b, c]` such that  `a² + b² =  c²`. Use
--    the `guard`  function in  an array  comprehension to
--    write  a function  `triples`  which  takes a  number
--    `n`  and calculates  all  Pythagorean triples  whose
--    components are  less than `n`. Your  function should
--    have type `Int -> Array (Array Int)`.
pythagoreanTriple :: Int -> Array (Array Int)
pythagoreanTriple n = do
  i <- 1 .. n
  j <- 1 .. n
  k <- 1 .. n
  guard $ (i*i + j*j) == (k*k)
  pure [i,j,k]

pythagoreanTriple' :: Int -> Array (Array Int)
pythagoreanTriple' n = filter (unsafePartial tripleCheck) $
  concatMap (\i -> concatMap (\j -> map (\k -> [i,j,k]) (1 .. n)) (1 .. n)) (1 .. n)
  where
    tripleCheck :: Partial => Array Int -> Boolean
    tripleCheck [i,j,k] = (i*i + j*j) == (k*k)

-- 4. (Difficult) Write a  function `factorizations` which
--    produces  all _factorizations_  of  an integer  `n`,
--    i.e.  arrays  of  integers  whose  product  is  `n`.
--    _Hint_:  for an  integer greater  than 1,  break the
--    problem down into two subproblems: finding the first
--    factor, and finding the remaining factors.

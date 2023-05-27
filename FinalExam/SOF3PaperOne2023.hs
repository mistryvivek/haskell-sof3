module SOF3PaperOne2023 where

import Data.List(sort, sortBy)
import Data.Maybe (fromJust)
import Data.Char(digitToInt)
import Data.Function(on)

{-
# Software 3: Summative assessment, Summer term 2022-23

## Paper 1: 3 hours (50 Marks)

Question numbers range from Question 1 to Question 8.

You may load any modules defined in the [`base`
package](https://hackage.haskell.org/package/base).

You should only replace `undefined` on the right-hand side of a
definition.  All other text should be left as is; alterations may lead
to a zero mark.

Values to be defined have names related to the question number, not to
the purpose of the value.  To clarify some questions we have provided
an indicative use of the value, with name of the form `test_...`; it
is not enough to pass the tests to gain full marks.

The questions total 45%.  The remaining 5% of the marks are available
for good programming style, including, but not necessarily limited to:
* simplicity of code,
* reuse of existing solutions, including values defined in modules
   contained in the [`base`
   package](https://hackage.haskell.org/package/base) (such as the
   loaded-by-default `Prelude` module), and
* comments.

-}

{-
## Question 1 [3 marks]

Define a function that has as input three `Integer`s and as output a
triple of the same integers, ordered with the lowest first and the
highest last.
-}
q1 :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
q1 a b c = formatTuple $ sort [a, b, c]
  where formatTuple [x,y,z] = (x,y,z) 
  -- No need to complete all pattern matches due the type definition forcing three integers.
test_q1 :: Bool
test_q1 = q1 1 2 3 == (1, 2, 3) && q1 3 2 1 == (1, 2, 3)

{-
## Question 2 [3 marks]

A **Pythagorean triple** consists of three non-negative integers, such
that the sum of the squares of the two smallest values equals the
square of the largest value (that is, they can be the sides of a
right-angled triangle).

Define a function to test if three non-negative `Integer` values form
a Pythagorean triple.
-}

--EXAM ASSUMPTION: It was never stated that the integers will always
--be entered in ascending size order hence they have been sorted beforehand.

q2 :: Integer -> Integer -> Integer -> Bool
q2 a b c = (head orderedNumbersSquared + orderedNumbersSquared !! 1) == orderedNumbersSquared !! 2
  where orderedNumbersSquared = map (\x -> x * x) $ sort [a, b, c]
test_q2 :: Bool
test_q2 = q2 3 4 5 && not(q2 1 2 3)

{-
## Question 3 [4 marks]

Define a function to check if a given value is present in every list
in a list of lists.  `False` should only be returned if there is some
list that does not contain the given value.
-}
q3 :: Eq a => a -> [[a]] -> Bool
q3 y xss = and [y `elem` xs | xs <- xss]
test_q3 :: Bool
test_q3 =    q3 'a' ["all", "lambs", "baa"]
          && not (q3 0 [[0], [1]])
          && q3 "Value" []
{-
## Question 4 [5 marks]

Define a function that receives as input two lists.

The first list is a list of pairs, `conversionTable :: [(a, a)]`.  You
may assume that the first elements of each pair in the list are not
repeated as first elements; that is `map fst conversionTable` has no
repeated elements.  If the function is called with an argument that
breaks this assumption its behaviour is not specified.

The second list is a list of elements, with type `[a]`.

The output of the function is the second input list, but with any
element that occurs as the first element of a pair in the
conversion table replaced by the second element of the pair.
-}
q4 :: Eq a => [(a, a)] -> [a] -> [a]
q4 table = map lookup'
  -- If "Nothing" is returned by the lookup, it will default to the original value. 
  where lookup' x = maybe x id (lookup x table) 
test_q4 :: Bool
test_q4 =    q4 [('a', 'A'), ('b', 'R'), ('c', 'C')] "mini cab" == "mini CAR"
          && q4 [(0,9), (2,9) , (4,9)] [0 .. 5] == [9, 1, 9, 3, 9, 5]

{-
## Question 5 [5 marks]

Define a function that has as inputs:
1. a base, in the range `[2 .. 10]`, and
2. a `String` of digits (a digit is a `Char`acter in the range `['0' .. '9']`)

and which outputs the number represented by the digit string, in that base.
-}
q5 :: Int -> String -> Int
q5 base = foldl (\x y -> base * x + digitToInt y) 0 
test_q5 :: Bool
test_q5 = q5 2 "100" == 4 -- 100 (base 2) is 4 (base 10)
        && q5 5 "401" == 101 -- 401 (base 5) is 101 (base 10)

{-
## Question 6 [6 marks]

Define a function that converts an `Int` to a string of digits in a
given base, with the same assumptions as in Question 5.  The first
input is the base, and the second argument is the integer to convert
to a digit-string.
-}
q6 :: Int -> Int -> String
q6 _ 0 = []
q6 base deci = let (quotient, remainder) = deci `divMod` base
               in q6 base quotient ++ show remainder 

test_q6 :: Bool
test_q6 = q6 10 123 == "123" && q6 2 6 == "110"


{-
## Question 7 [12 marks]

Consider the following type of binary trees:
-}
data BinTree a = Leaf | Node (BinTree a) a (BinTree a)
  deriving Show
{-
Define a function to convert a `BinTree` to a list in **breadth-first**
order.
-}

-- Traverse while saving nodes in using the tuple pair: (depth, value). 
q7 :: BinTree a -> [a]
q7 ts = map snd (sortBy (compare `on` fst) (withDepth 0 ts))
  where withDepth n Leaf = []
        withDepth n (Node x y z) = [(n+1, y)] ++ withDepth (n+1) x ++ withDepth (n+1) z

test_q7 :: Bool
test_q7 =
  let
    bt = Node (Node (Node Leaf
                          3
                          Leaf)
                    1
                    (Node (Node Leaf
                                6
                                Leaf)
                          4
                          Leaf))
              0
              (Node Leaf
                    2
                    (Node Leaf
                          5
                          Leaf))
  in
       q7 bt == [0 .. 6] -- breadth-first order
    && q7 bt /= [0, 1, 3, 4, 6, 2, 5]-- depth-first order

{-
## Question 8 [7 marks]

Define a function that generates permutations of lists.

The inputs are:

1. A permutation, represented as a bijection (that is, a total,
   one-to-one, onto function) from `[0 .. n - 1]` to itself, where `n`
   is the length of the list to be permuted.  The permutation maps
   **from new** indices **to old** indices.
2. the list to be permuted.

A permutation is a function that maps **from new** indices **to old** indices.
-}
q8 :: (Int -> Int) -> [a] -> [a]
q8 fn xs = map translate convertedUnits
  where convertedUnits = [fn x | x <- [0..length xs-1]]
        translate x = xs !! x


test_q8 :: Bool
test_q8 =
  let -- example permutation
    p 0 = 3
    p 1 = 1
    p 2 = 0
    p 3 = 2
  in -- example use of a permutation
    q8 p "abcd" == "dbac"

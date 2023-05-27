module Formative2 where
import Data.List(nub)
type REPLACE_THIS_TYPE = () -- DO NOT ALTER

{-
# Software 3: Formative Assessment 2

## 3 hours (50 Marks)

You may load any modules defined in the [`base`
package](https://hackage.haskell.org/package/base).

You should only replace `undefined` and `REPLACE_THIS_TYPE` on the
right-hand side of a definition.  All other text should be left as is;
alterations may lead to a zero mark.

Values to be defined sometimes have names related to the question
number, not to the purpose of the value.  To clarify some questions we
have provided an indicative use of the value, with name of the form
`test_...`; it is not enough to pass the tests to gain full marks.

The questions total 45%.  The remaining 5% of the marks are available
for good programming style, including, but not necessarily limited to:
* simplicity of code,
* reuse of existing solutions, including values defined in modules
   contained in the [`base`
   package](https://hackage.haskell.org/package/base) (such as the
   loaded-by-default `Prelude` module), and
* comments.

## Question 1 [20 marks]

In this question you are asked to define types, as well as values.
Each type, *Name*, is given a default implementation: `type Name =
REPLACE_THIS_TYPE`.  You may change the keyword `type` to either
`newtype` or `data` if appropriate, and you should replace
`REPLACE_THIS_TYPE`, by a suitable type expression.  You may derive any
type classes, such as `Show`, that will be useful.

Marks will be mostly given for how well the type protects the
programmer from mistakes, for its utility to the programmer, and also
partly for the run-time efficiency of the type.

This question relates to preparing a play list of songs
for a radio programme.

A programme consists of segments.
The total time of songs scheduled should not exceed
the time available in a segment.

The time required for a song includes the time to introduce it.

All times are given in whole numbers of seconds.

No song may be repeated in a programme.
Songs are identified by an `Int`eger ID,
which is guaranteed to be different for different songs,
even if the titles are the same.

A record is represented by:
-}
data Song = Song {ident :: Int, title :: String, duration :: Int}
  deriving (Show)

{-
### 1(i) [3 marks]

Instantiate `Song` as a member of `class Eq`.

-}

instance Eq Song where
  (==) a b = ident a == ident b 

{-
### 1(ii) [7 marks]

Define suitable types to represent segments and programs.
Programmes are collection of segments,
and segments have durations (in whole numbers of seconds)
as well as a collection of songs to be played during the segment.
-}

newtype Programme = Programme [Segment]
type Segment = ([Song], Int)

{-
### 1(iii) [10 marks]

Define functions to determine if songs, segments and programmes are valid:
* a song is valid if its title is not empty, and its duration is positive,
* a segment is valid if the total time taken by the songs fits into the duration of the segment and all the songs are valid.
* a programme is valid if all its segments are valid, and no song is repeated.

-}
validSong :: Song -> Bool
validSegment :: Segment -> Bool
validProgramme :: Programme -> Bool
validSong sg = not $ null (title sg) && duration sg >= 0
validSegment (sgs, dr) = all validSong sgs && dr >= segdr
  where segdr = sum $ map duration sgs
validProgramme (Programme x) = all validSegment x && allSongs == nub allSongs
  where allSongs = concatMap getAllSongs x 
        getAllSongs (a , _) = map ident a

{-
## Question 2 [15 marks]

The Society Of Fast Food Fanciers wants to keep a database of food
suppliers, with a recommendation for each food type sold by those
suppliers.

A recommendation is captured by the data type `Recommendation`:

-}
data Recommendation = Bad | Poor | Good | Excellent
  deriving (Eq, Show)
{-
They are considering two different representations,
generic in the type of outlet names and food names.

The first keeps a list of triples:
-}
newtype RecL outlet food = RecL [((outlet, food), Recommendation)]
{-
For example, assumming suitable instantiations of `shop` and `food`:
```haskell
RecL [((RCH, Pizza), Poor), ((PZA, Burrito), Good)]
```

The second stores the recommendations in a function:
-}
newtype RecF outlet food = RecF ((outlet, food) -> Maybe Recommendation)
{-
For example:
```haskell
RecF (\ otfd -> case otfd of
                  (RCH, Pizza)   -> Just Poor
                  (PZA, Burrito) -> Just Good
                  _              -> Nothing)
```

For each of the tasks 2(i), 2(ii) and 2(iii) below, give **two**
solutions, one using each type.

### 2(i) [2 marks]

Give values to represent an empty database:

-}
emptyL :: RecL outlet food
emptyF :: RecF outlet food
emptyL = RecL []
emptyF = RecF (const Nothing)

{-
### 2(ii) [7 marks]

Give functions to enter a new recommendation.
If a recommendation for the given food at the given outlet already exists,
then the new recommendation replaces the old one.

-}
enterL :: (Eq outlet, Eq food) =>
          outlet -> food -> Recommendation -> RecL outlet food -> RecL outlet food
enterF :: (Eq outlet, Eq food) =>
          outlet -> food -> Recommendation -> RecF outlet food -> RecF outlet food
enterL o f rect (RecL db) = RecL (((o, f), rect) : filter (\((x, y), _) -> x == o && y == f) db)
enterF o f rect (RecF fn) = undefined

{-
### 2(iii) [6 marks]

Give functions to report a recommendation,
if there is one, and `Nothing` otherwise:
-}
reportL :: (Eq outlet, Eq food) =>
           outlet -> food -> RecL outlet food -> Maybe Recommendation
reportF :: (Eq outlet, Eq food) =>
           outlet -> food -> RecF outlet food -> Maybe Recommendation
reportL o f (RecL x) = lookup (o, f) x 
reportF o f (RecF x) = x (o, f)

{-
## Question 3 [10 marks]

Recall the definition of the `ProofLayout` type constructor:
-}
infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
{-
Consider the `Prelude` functions:

```haskell
length :: [a] -> Int
length []     = 0             -- length.0
length (_:xs) = 1 + length xs -- length.1

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys             -- (++).0
(x:xs) ++ ys = x : (xs ++ ys) -- (++).1
```

Prove that length distributes over concatenation:
```haskell
forall xs, ys :: [a] {length (xs ++ ys) == length xs + length ys}
```
You may assume
```haskell
0 + n == n -- (+).unit
```

Hint: use structural induction on the first parameter of `lenDistConcat`.

You may assume the following strictness laws:
```haskell
  length _|_ = _|_ -- length strict
  _|_ ++ ys = _|_ -- (++) left-strict
  _|_ + n = _|_ -- (+) left-strict
```

-}
lenDistConcat :: [a] -> [a] -> ProofLayout Int
lenDistConcat = undefined

module ExtendedExercise where
import Text.Read (readMaybe) -- for the runnable version

{-
# SOF3/Extended Exercise: The Royal Game of Ur

This exercise asks you to implement components for playing *The Royal
Game of Ur*.  It is provided as an extra programming project to allow
you to practice further.

Throughout this exercise you may find it useful to implement utility
functions that are not explicitely requested, as well as those that
are requested.

We have provided a few tests to
1. clarify what the functions are to do.
2. help you test your solutions.

Note that passing these tests is _not_ a guarantee that you have got
your definitions correct in any other cases.  You are encouraged to
test further.

The Royal Game of Ur is the oldest known board game.  The oldest known
board and pieces are around 4,500 years old.  The only rule book we
have, which is for versions of the game rather than the original game,
is a youthful 2,500 years or so old.  This rule book is written in
cuneiform letters on a clay tablet, and currently resides in [the
British Museum](https://www.britishmuseum.org/).  It was translated by
one of the British Museum's curators, Irving Finkel.  There are
YouTube videos of him [discussing](https://youtu.be/wHjznvH54Cw) and
[playing the game with Tom Scott](https://youtu.be/WZskjLq040I) (Tom
Scott is a linguistics graduate from the University of York, ex-YUSU
president, and runs a YouTube channel).

The rules are not completely known, but this exercise uses rules based
on the [Finkel-Scott match](https://youtu.be/WZskjLq040I).

The game is a race game, such as ludo or backgammon, for two players,
that we will call `Red` and `Green`.
-}
data Player = Red | Green deriving (Eq, Show)

{-
A board contains 14 _logical_ squares, arranged as 20 _physical_ squares.

```
-----------------------------------------                   ---------------------
| *Sq_4   |  Sq_3   |  Sq_2   |  Sq_1   |                   | *Sq14   |  Sq13   | 
---------------------------------------------------------------------------------
|  Sq_5   |  Sq_6   |  Sq_7   | *Sq_8   |  Sq_9   |  Sq10   |  Sq11   |  Sq12   | 
---------------------------------------------------------------------------------
| *Sq_4   |  Sq_3   |  Sq_2   |  Sq_1   |                   | *Sq14   |  Sq13   | 
-----------------------------------------                   ---------------------
```

In addition there are two "locations" off of the board:
* the start, where pieces wait to move onto the board
* home, where pieces are when they have finished their journey around the board.

-}

data Position = Start -- off board
              | Sq_1 | Sq_2 | Sq_3 | Sq_4 -- private
              | Sq_5 | Sq_6 | Sq_7 | Sq_8 | Sq_9 | Sq10 | Sq11 | Sq12 -- shared
              | Sq13 | Sq14 -- private
              | Home -- off board
              deriving -- type classes that may be useful in your solution
                (Eq, Ord, Enum, Bounded, Show)

redBoard :: [Position]
greenBoard :: [Position]
redBoard = [Start .. Sq12] ++ [Home]
greenBoard = [Start] ++ [Sq_5 .. Sq14] ++ [Home]


{-
The red player's pieces move along the top and middle rows, in
numerical order, while the green player's pieces move similarly along
the bottom and middle rows.  Squares 1-4 and 13-14 are private, but
squares 5-12 are _shared_ and where, in Irving Finkel's words, the two
players are "at war".

There are special squares: the private squares 4 and 14, and the
shared square 8.  On a real board these are decorated with a rosette,
indicated above by an asterisk ('*').

Utilities for checking the kind of `Position`:
-}
isShared, isRosette, isSharedRosette, notHome :: Position -> Bool
isShared        pos = Sq_5 <= pos && pos <= Sq12
isRosette           = (`elem` [Sq_4, Sq_8, Sq14])
isSharedRosette pos = isShared pos && isRosette pos
notHome             = (/= Home)

{-
Each player has seven (7) identical pieces.  Each piece has a
position:
* waiting to enter the board (at the `Start`),
* on a square on the board, or
* having reached `Home`.

-}
piecesPerPlayer :: Int
piecesPerPlayer = 7

{-
The dice used are essentially four tossed coins that could be 1
(heads) or 0 (tails) each, and the value of the "throw" is the sum.

This gives probabilities:
* 0 → 1/16
* 1 → 4/16 = 1/4
* 2 → 6/16 = 3/8
* 3 → 4/16 = 1/4
* 4 → 1/16

You are not asked to implement the dice: when rolls are needed these
will be provided by an external oracle.
-}


{-
We want to know where pieces are on the board.  To do this we use a
function which answers the question.
-}
type Placement = (Position, Player) -> Int
{-
If `p :: Placement` then the expression `p (Sq_3, Red)` is the number of
pieces that `Red` has in position `Sq_3`.  Similarly, `p (Home, Green)` is
the number of pieces that `Green` has that have reached `Home`.

The state of a game is a `Placement` and the `Player` whose turn it is.

-}
data GameState = GameState Placement Player
{-
---

## Task 1

Implement the utility function `opponent` that returns a player's opponent.
-}
opponent :: Player -> Player
opponent Red = Green
opponent Green = Red
test_opponent :: Bool
test_opponent = opponent Red == Green
{-
## Task 2

Implement the utility function `isValidRoll` that checks a dice roll for being in range.

-}
isValidRoll :: Int -> Bool
isValidRoll a = a `elem` [0..4] 
test_isValidRoll :: Bool
test_isValidRoll  = isValidRoll 2 && not (isValidRoll 9)

{-
## Task 3

Implement the utility function `plus` that adds a dice roll to a
position to get a new position (values that go beyond `Home` should be
treated as `Home`).

-}
plus :: Position -> Int -> Position
plus pos dice = restOfBoard !! dice
  where restOfBoard = [pos..Home] ++ repeat Home
        
        
test_plus :: Bool
test_plus =    Start `plus` 0 == Start
            && Start `plus` 3 == Sq_3
            && Sq_3  `plus` 2 == Sq_5
            && Sq14  `plus` 4 == Home
{-

## Task 4

Implement a pair of functions, `fromList` and `toList` that convert
between lists and `Placement`s.  They should be inverses of each other.
-}
toList :: Placement -> [((Position, Player), Int)]
fromList :: [((Position, Player), Int)] -> Placement
toList = undefined
fromList = undefined 
testToFromList :: Bool
testToFromList = ((Sq_3, Red), 9) `elem` toList(fromList [((Sq_3, Red), 9)])
                 && not(((Sq10, Red), 0) `elem` toList(fromList [((Sq_3, Red), 9)]))
{-
We can now instantiate `GameState` as an instance of `Eq`:

-}
instance Eq GameState where
  GameState p x == GameState q y = (toList p, x) == (toList q, y)
{-

## Task 5

The pair of functions `toList`/`fromList` can take inputs that do not
represent a valid placing of pieces: for example, there may not be the
right number of tokens for `Red` or `Green`, or there may be too many
pieces associated with a position.

Implement a pair of functions `validList` and `validPlacement` that
check for validity.

A placement is valid exactly when:
1. Each player has 7 pieces on the board (including `Start` and `Home`).
2. At most one piece can be on a shared square (`Sq_5` to `Sq12` inclusive).
3. At most one piece per player may be on an unshared square (`Sq_1` to `Sq_4` inclusive, plus `Sq13` and `Sq14`).
4. `Start` and `Home` may have any number of pieces on them, subject to Rule 1.

-}
validPlacement :: Placement -> Bool
validList :: [((Position, Player), Int)] -> Bool
validPlacement = undefined
validList = undefined

test_validPlacement :: Bool
test_validPlacement = validPlacement plac0 && not (any validPlacement [plac1, plac2])
  where
    plac0 (Start, Red)   = 6
    plac0 (Sq_3, Red)    = 1
    plac0 (Start, Green) = 5
    plac0 (Home, Green)  = 2
    plac0 _              = 0
    --
    plac1 (Start, Red)   = 9
    plac1 (Start, Green) = 7
    plac1 _              = 0
    --
    plac2 (Start, Red)   = 6
    plac2 (Start, Green) = 7
    plac2 _              = 0
{-
## Task 6

The initial state has both players with all their tokens at the start.
The first move belongs to the "red" player.  Implement this state.
-}
initGS :: GameState
initGS = undefined

test_initGS_placement :: Bool
test_initGS_placement = validPlacement plac
                        && plac (Start, Red) == 7
                        && plac (Sq10, Green) == 0
                        && plac (Home, Red) == 0
                        && rd == Red
  where
    GameState plac rd = initGS

{-
## Task 7
A move from a chosen position by the current player is possible if:
1. The chosen position is not `Home`.
2. There is a piece belonging to the current player on the chosen position.
3. There is not already a piece belonging to the current player in the
   new position, unless the new position is `Home`.
4. The new position is not the shared rosette currently occupied by
   the other player.

Implement the function `possibleMoves` that returns all the squares
from which the current player has a possible move.  The function may
assume that the input dice roll is in the valid range.  When the roll
is `0` there is an ambiguity: are there no moves, or can any piece be
"moved" to the square it is already on?  You should report no possible
moves.
-}
possibleMoves :: GameState -> Int -> [Position]
possibleMoves = undefined
test_possibleMoves :: Bool
test_possibleMoves =    possibleMoves initGS 0 == []
                     && possibleMoves initGS 3 == [Start]

{-
## Task 8

Now implement a function `move` that takes a `GameState` and a
dice-roll/position-to-move-from pair and returns a new `GameState`.
1. If any input is invalid, (an invalid dice roll, or the roll is
   valid but it is not possible to move that distance from the
   nominated position) then the game state does not change.
2. If the current dice roll is valid, then
   1. If there are valid moves with the dice roll:
      1. The current player chooses one.
      2. The player's token is moved from the chosen position to the new
         position.
      3. If the new position is a shared square, and it is occupied by
         the other player then the other player's piece returns to the
         start.
      4. The next player is the other player, unless the new position is
       a rosette.
   2. If the dice roll has no valid moves, the placement does not
       change, but the next player is the other player

-}

move :: GameState -> (Int, Position) -> GameState
move = undefined

test_move :: Bool
test_move =    plac1 (Start, Red) == pred piecesPerPlayer
            && plac1 (Sq_1, Red) == 1
            && plac1 (Sq_2, Red) == 0
            && plac1 (Start, Green) == piecesPerPlayer
            && plac1 (Sq_1, Green) == 0
            && plr1 == Green
            && plac2 (Start, Red) == pred piecesPerPlayer
            && plac2 (Sq_1, Red) == 1
            && plac2 (Sq_2, Red) == 0
            && plac2 (Start, Green) == pred piecesPerPlayer
            && plac2 (Sq_1, Green) == 0
            && plac2 (Sq_2, Green) == 1
            && plr2 == Red
            && plac2' (Start, Green) == piecesPerPlayer
            && plac2' (Sq_1, Red) == 1
            && plac2' (Sq_2, Red) == 0
            && plac2' (Sq_2, Green) == 0
            && plr2' == Green
  where
    gs1 = move initGS (1, Start)
    GameState plac1  plr1  = gs1
    GameState plac2  plr2  = move gs1 (2, Start)
    GameState plac2' plr2' = move gs1 (5, Start)

{-
## Task 9

Implement a function, `gameOver` to report if the game has been won or
not, and if won, then who the winner is.
-}
gameOver :: GameState -> Maybe Player
gameOver = undefined

test_gameOver :: Bool
test_gameOver =    gameOver initGS == Nothing
{-
Further tests are in Q10.
-}

{-
## Task 10

Implement a function, `playSequence`, that calculates the result of a
finite series of moves, starting from the initial state.
-}
playSequence :: [(Int, Position)] -> GameState
playSequence = undefined

test_playSequence_gameOver :: Bool
test_playSequence_gameOver =
     gameOver (playSequence [])                      == Nothing
  && gameOver (playSequence [(4, Start), (4, Sq_4)]) == Nothing
  && gameOver (playSequence (take 40 seq1))          == Nothing
  && gameOver (playSequence (take 41 seq1))          == Just Red
  && gameOver (playSequence (take 42 seq2))          == Just Green
  where
    seq1 = cycle [(4, Start), (4, Sq_4), (4, Sq_8), (0, Start), (4, Sq12), (0, Start)]
    seq2 = (0, Start) : seq1


{-
## Interface

This section includes code to animate the solutions here.  It ought to
work "out-of-the-box" with your own code.

Evaluating `playRGU` allows a pair of users to play The Royal Game of
Ur.  You will need to implement your own dice (the count of heads in
four tossed coins works).

-}

-- Utilities to pretty print a placement and a game state
ppPlayer :: Player -> String
ppPlayer Red   = "R"
ppPlayer Green = "G"

ppPlacement :: Placement -> String -- prettyprint a placement
ppPlacement plac =
  unlines
  [homestart Red,
   houter,
   privaterow Red,
   hinner,
   cells [Red, Green] [Sq_5 .. Sq12],
   hinner,
   privaterow Green,
   houter,
   homestart Green]
  where
    privaterow :: Player -> String
    privaterow pl =
      cells [pl] (reverse [Sq_1 .. Sq_4])
      ++ replicate 18 ' '
      ++ cells [pl] [Sq14, Sq13]
    cells :: [Player] -> [Position] -> String
    cells ps sqs = "| " ++ foldr (\ s t -> s ++ " | " ++ t) "" (map content sqs)
      where
        content pos =    (if isRosette pos then "*" else " ")
                      ++ show pos
                      ++ " "
                      ++ playerPresent pos
          where
            playerPresent pos = [head (concat (map (showPlayer pos) ps) ++ " ")]
              where
                showPlayer pos plr | plac (pos, plr) == 1 = ppPlayer plr
                                   | otherwise            = ""

    hinner = replicate 81 '-'
    houter = replicate 41 '-' ++ replicate 19 ' ' ++ replicate 21 '-'
    homestart pl =    show pl ++ " start:   " ++ show (plac (Start, pl))
                   ++ replicate 5 ' '
                   ++ show pl ++ " home:    " ++ show (plac (Home,  pl))

ppGS :: GameState -> String -- prettyprint a game state
ppGS (GameState plac plr) = "\nBoard:\n" ++ ppPlacement plac
                            ++ "\nTo play: " ++ show plr
                            ++ "\n"

-- utility to get Int in range minLimit-maxLimit 
getInt :: String -> Int -> Int -> IO Int
getInt prompt minLimit maxLimit = body
  where
    body = do
      putStr ("Enter " ++ prompt ++ " ("
              ++ show minLimit ++ "-" ++ show maxLimit
              ++ ") or <return> to abort the game: ")
      input <- getLine
      checkVal input
        where
          checkVal "" = do
            putStr "Confirm quit (Y to quit, anything else to continue): "
            response <- getLine
            if response == "Y"
              then error "Game aborted"
              else body
          checkVal inp = maybe onError checkRoll (readMaybe inp)
            where
              onError = do
                putStrLn ("Invalid input: " ++ inp)
                body
              checkRoll n | 0 <= n && n <= maxLimit = pure n
                          | otherwise               = onError

{-
playRGU :: IO ()
playRGU = body initGS
  where
    body gs = maybe continue done (gameOver gs)
      where
        continue = do
          putStrLn (ppGS gs)
          roll <- getInt "a dice roll" minRoll maxRoll
          let pm = possibleMoves gs roll
          if roll == 0 || null pm
            then do
                   putStrLn "Missed turn"
                   let GameState b p = gs
                   body (GameState b (opponent p))
            else do
                   putStrLn ("Tokens to move are: " ++ show (zip [0..] pm))
                   id <- getInt "the ID of a token position" 0 (length pm - 1)
                   body (move gs (roll, pm!!id))
        done pl = putStrLn ("Congratulations: " ++ show pl ++ " wins!")
-}
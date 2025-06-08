-- Copyright (c) 2025, Andres Löh, Well-Typed LLP
module Day1 where

-- Let's say we want to model aspects of a card game.
-- Cards have colours (yellow, red, purple, or black)
-- and a rank which is one, two or three.

-- *** Step 1. Let's define a datatype for card colours.

data CardColour =
    Yellow
  | Red
  | Purple
  | Black
  deriving (Eq, Show)

-- >>> Yellow
-- No instance for `Show CardColour' arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_a1GM

-- *** Step 2. To work with card colours conveniently,
-- it is helpful if we can evaluate expressions that compute
-- colours and print the results to screen. In order to do
-- that, GHC has to know what string to associate with
-- each of the data constructors of the type.
--
-- One way to do this is to define a function for this ...

showColour :: CardColour -> String
showColour Yellow = "green"
showColour Red    = "Red"
showColour Purple = "Purple"
showColour Black  = "Black"

-- >>> showColour Yellow
-- "Yellow"

-- instance Show CardColour where
--   show = showColour

-- >>> Yellow
-- Yellow

--
-- And GHC can now use other functions that are internally
-- using 'show' also on the type 'CardColour'.

-- *** Step 4. Let's see if we can evaluate values of this
-- type and ask for their inferred types. We can use the
-- eval plugin or GHCi for this.
--
-- >>> :t Yellow
-- Yellow :: CardColour

-- *** Step 5. An easier way to achieve the same thing is
-- to simply write 'deriving Show' at the end of the 'data'
-- declaration. This is possible for select type classes,
-- if the implementation is "obvious" from the datatype
-- declaration itself. 'Show' is one of these classes.
-- Let's try this out for another datatype, namely ranks
-- of cards. We could use a number type such as 'Int', but
-- if we have three choices only such as here, that would
-- be very imprecise. So let's use a dedicated type with
-- exactly three choices and derive 'Show'.

data Rank =
    Rank1
  | Rank2
  | Rank3
  deriving (Eq, Show)

-- *** Step 6. Colours and ranks are associated with a
-- point score. Let's start with colours:
-- Yellow is worth 10 points, red 20, purple 30 and black 100.
-- Let's define a function that captures this.

colourScore :: CardColour -> Int
colourScore Yellow = 10
colourScore Red    = 20
colourScore Purple = 30
colourScore Black  = 100

--
-- Note that a type signature is optional, but strongly
-- recommended.

-- *** Step 7. Ranks also map to scores. This
-- is the obvious mapping, e.g. rank 2 is associated with
-- 2 points. Let's define a function for this.

rankScore :: Rank -> Int
rankScore Rank1 = 1
rankScore Rank2 = 2
rankScore Rank3 = 3

-- *** Step 8. Actual cards have a colour and a rank.
-- Let's define a datatype that captures this.

data Card =
  MkCard { cardColour :: CardColour, cardRank :: Rank }
  deriving (Eq, Show)

-- >>> MkCard Red Rank2
-- MkCard {cardColour = Red, cardRank = Rank2}
-- >>> :t MkCard Red Rank2
-- MkCard Red Rank2 :: Card
-- >>> :t MkCard
-- MkCard :: CardColour -> Rank -> Card
-- >>> :t MkCard Red
-- MkCard Red :: Rank -> Card
-- >>> :t MkCard Red Rank2
-- MkCard Red Rank2 :: Card

-- *** Step 9. The score of a card is the sum of the
-- score of its colour and the score of its rank.
-- Let's define a function for this.

scoreCard :: Card -> Int
scoreCard (MkCard colour rank) = colourScore colour + rankScore rank

-- *** Step 10. Sometimes we also want to access the
-- colour and the rank of a given card. We can always
-- use pattern matching, but we can also define dedicated
-- selector functions.

-- cardColour :: Card -> CardColour
-- cardColour (MkCard colour _rank) = colour
--
-- cardRank :: Card -> Rank
-- cardRank (MkCard _colour rank) = rank

-- This can more easily be done with records.

-- *** Step 11. Let's define a more compact textual
-- representation for cards. It's tempting but usually not
-- advisable to use the 'Show' class for this. So we just
-- define separate functions for colours and cards that abbreviates
-- colours by single letters and uses a dash to combine a
-- colour with a rank. For ranks, we could define another
-- function as well, but we can also use 'show' on the score
-- of the rank.

compactColour :: CardColour -> String
compactColour Yellow = "Y"
compactColour Red    = "R"
compactColour Purple = "P"
compactColour Black  = "B"

compactCard :: Card -> String
compactCard (MkCard colour rank) =
  compactColour colour ++ "-" ++ show (rankScore rank)

-- >>> :t show
-- show :: Show a => a -> String
-- >>> (+) 2 5
-- 7
-- >>> (++) "foo" "bar"
-- "foobar"
-- >>> :t (+)
-- (+) :: Num a => a -> a -> a
-- >>> :t (++)
-- (++) :: [a] -> [a] -> [a]

-- *** Step 12. A "combination" is either a pair or a triple
-- of cards. Let's define a datatype for this.

data Combination =
    Pair Card Card
  | Triple Card Card Card
  deriving Show

-- >>> :t Pair
-- Pair :: Card -> Card -> Combination
-- >>> :t Triple
-- Triple :: Card -> Card -> Card -> Combination

exampleCard :: Card
exampleCard = MkCard Red Rank2

-- >>> :t Triple exampleCard exampleCard
-- Triple exampleCard exampleCard :: Card -> Combination

-- *** Step 13. Combinations also have scores.
-- The score of a combination
-- is the sum of the cards involved, except if all of them
-- have the same colour: then the original score is doubled.
-- We can first define a helper function for the plain score,
-- which is always the sum.

plainScore :: Combination -> Int
plainScore (Pair card1 card2) =
  scoreCard card1 + scoreCard card2
plainScore (Triple card1 card2 card3) =
  scoreCard card1 + scoreCard card2 + scoreCard card3

-- *** Step 14. We need a way to check if two cards are
-- of the same colour. We can define this by pattern
-- matching as well.

equalColour :: CardColour -> CardColour -> Bool
equalColour Yellow Yellow = True
equalColour Red    Red    = True
equalColour Purple Purple = True
equalColour Black  Black  = True
equalColour _      _      = False

-- instance Eq CardColour where
--   (==) = equalColour
--

-- data Bool =
--     False
--   | True

-- Interlude: Multiple arguments, currying. (If this has
-- not already happened.)

-- *** Step 15. Equality is a common concept, so there is
-- another type class for this. And the definition is again
-- obvious from the datatype definition, so actually it
-- can also be derived. So let's add "deriving Eq" to our
-- datatype definitions above. Now we can use the '=='
-- operator to test equality on all of these types.

-- >>> Purple == Black
-- False
-- >>> MkCard Red Rank2 == MkCard Red Rank2
-- True

-- *** Step 16. We can now define a function to compute
-- the actual score of a combination. Again, it's probably
-- helpful to define one more helper.
-- (guards, if-then-else, Bool, case, let, where)

sameColourCombination :: Combination -> Bool
sameColourCombination (Pair card1 card2) =
  cardColour card1 == cardColour card2
sameColourCombination (Triple card1 card2 card3) =
  cardColour card1 == cardColour card2 && cardColour card2 == cardColour card3

-- *** Step 17. A stack of cards is a (possibly)
-- empty number of cards placed on top of one another.
--
-- One way to model this is:
-- - a stack can be empty;
-- - if a stack isn't empty, there's a top card and a (smaller)
--   stack below.

data Stack =
    Empty
  | Card `OnTopOf` Stack
  deriving Show

infixr 5 `OnTopOf`

-- >>> min 3 5
-- 3
-- >>> 3 `min` 5
-- 3

-- *** Step 18. Let's define a function that counts
-- the number of cards in a stack.

stackSize :: Stack -> Int
stackSize Empty                   = 0
stackSize (_card `OnTopOf` stack) = 1 + stackSize stack

-- Interlude: standard design pattern

-- *** Step 19. Sometimes, we want to remove all cards
-- of a given colour from a stack of cards, keeping
-- the others in their original order. Let's
-- write a function for this.

removeColourFromStack :: CardColour -> Stack -> Stack
removeColourFromStack _colour Empty = Empty
removeColourFromStack colour (card `OnTopOf` stack)
  | cardColour card == colour = removeColourFromStack colour stack
  | otherwise                 = card `OnTopOf` removeColourFromStack colour stack

-- >>> otherwise
-- True

-- removeColourFromStack colour (card `OnTopOf` stack) =
--   if cardColour card == colour
--     then ...
--     else ...
--
-- removeColourFromStack colour (card `OnTopOf` stack) =
--   case cardColour card == colour of
--     True -> ...
--     False -> ...

cardHasColour :: CardColour -> Card -> Bool
cardHasColour colour card = cardColour card == colour

-- *** Step 20. Sometimes, we want to flip a stack,
-- reversing the order of all the cards.
-- (accumulator, let/where, partial parameterisation)

reverseStack :: Stack -> Stack
reverseStack stack = reverseStackAux Empty stack

reverseStackAux :: Stack -> Stack -> Stack
reverseStackAux acc Empty = acc
reverseStackAux acc (card `OnTopOf` stack) =
  reverseStackAux (card `OnTopOf` acc) stack

-- >>> reverseStack (exampleCard `OnTopOf` (MkCard Black Rank3 `OnTopOf` Empty))
-- MkCard {cardColour = Black, cardRank = Rank3} `OnTopOf` (MkCard {cardColour = Red, cardRank = Rank2} `OnTopOf` Empty)

-- *** Step 21. We want to find the highest-valued
-- card in a stack. Such a card does not always exist
-- (as a stack can be empty). There is a datatype
-- defined for this already:
--
-- data Maybe a =
--     Nothing
--   | Just a
--
-- (Parameterised datatypes.)

-- *** Step 22. The concept of stacks is also very
-- common. It's a special case of Haskell's list type.
-- Lists are just parameterised stacks, but look
-- a bit different because they use special syntax.
--
-- data [a] =
--     []
--   | a : [a]
--
-- Let's define direct conversion functions between
-- 'Stack' and '[Card]' to establish the correspondence.

-- *** Step 23. Let's directly redefine flipStack on lists.
-- It doesn't make any assumptions about the type of elements,
-- so it can be (parametrically) polymorphic.

-- This function exists in the library as 'reverse'.

-- *** Step 24. Let's define a function that appends two lists.

-- This function exists in the library as '++'.

-- *** Step 25. The function that removes all cards of a given
-- colour can be generalised by abstracting over a predicate
-- on the elements. This is our first true higher-order function!

-- This function exists in the library as 'filter'.

-- *** Step 26. Let's now actually re-express 'removeColour'
-- via 'filterList' (but on '[Card]' rather than 'Stack').
-- (lambda expressions)

-- *** Step 27. Every card has a matching card. The matching
-- card is the one of the same colour so that the ranks add up
-- to 4. Let's define a function that computes the matching card
-- for a given card.

-- *** Step 28. For a stack of cards, let's compute the stack of
-- cards where every card has been replaced by its matching card.

-- *** Step 29. The idea to apply the same function to every element
-- in a list is very common. Let's define it in general on lists,
-- abstracting over the function to be applied.

-- *** Step 30. We want to define a list enumerating all possible cards.
-- Let's start by enumerating all possible colours and ranks.

-- (For simple enumeration types like this, but not for more complex
-- types, this can also be achieved by deriving the 'Bounded' and 'Enum'
-- type classes).

-- *** Step 31. Now we can use 'map' over these lists to
-- get all the cards as a list of lists.

-- *** Steps 32. Let's try to flatten such a list of lists into a
-- single list.

-- This function exists in the library as 'concat'.

-- *** Step 33. Now we can really get all the cards.

-- *** Step 34. What if we want the compact names for all these cards?
-- What if we want the scores of all these cards?

-- *** Step 35. Let's define a function on lists that checks whether
-- a particular element occurs in the list.
-- (There are many ways to achieve this now.)

-- This function is available in the library as 'elem'.

-- *** Step 36. Let's define a function that checks whether a given
-- score can occur as the score of a card combination.
--
-- Let's proceed in multiple steps:
--
-- - let's compute all pair combinations,
-- - let's compute all triple combinations,
-- - then we can compute all combinations,
-- - let's then compute all their scores,
-- - and finally define the function we want.


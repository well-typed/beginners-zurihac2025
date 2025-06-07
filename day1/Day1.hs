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
  deriving Eq -- see later

-- *** Step 2. To work with card colours conveniently,
-- it is helpful if we can evaluate expressions that compute
-- colours and print the results to screen. In order to do
-- that, GHC has to know what string to associate with
-- each of the data constructors of the type.
--
-- One way to do this is to define a function for this ...

showCardColour :: CardColour -> String
showCardColour Yellow = "Yellow"
showCardColour Red    = "Red"
showCardColour Purple = "Purple"
showCardColour Black  = "Black"

-- *** Step 3. And then to instantiate the 'Show' class
-- for 'CardColour'. This means we can use the name 'show'
-- to invoke 'showCardColour', but we can also use the same
-- name 'show' to turn other types into strings.
--
-- And GHC can now use other functions that are internally
-- using 'show' also on the type 'CardColour'.

instance Show CardColour where
  show = showCardColour

-- *** Step 4. Let's see if we can evaluate values of this
-- type and ask for their inferred types. We can use the
-- eval plugin or GHCi for this.

-- >>> Yellow
-- Yellow
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

data CardRank =
    Rank1
  | Rank2
  | Rank3
  deriving (Bounded, Enum, Eq, Show) -- for Bounded, Enum, Eq see later

-- *** Step 6. Colours and ranks are associated with a
-- point score. Let's start with colours:
-- Yellow is worth 10 points, red 20, purple 30 and black 100.
-- Let's define a function that captures this.
--
-- Note that a type signature is optional, but strongly
-- recommended.

colourScore :: CardColour -> Int
colourScore Yellow = 10
colourScore Red    = 20
colourScore Purple = 30
colourScore Black  = 100

-- >>> colourScore Blue
-- 30
-- >>> :t colourScore
-- colourScore :: CardColour -> Int
-- >>> :t colourScore Blue
-- colourScore Blue :: Int

-- *** Step 7. Ranks also map to scores. This
-- is the obvious mapping, e.g. rank 2 is associated with
-- 2 points. Let's define a function for this.

rankScore :: CardRank -> Int
rankScore Rank1 = 1
rankScore Rank2 = 2
rankScore Rank3 = 3

-- *** Step 8. Actual cards have a colour and a rank.
-- Let's define a datatype that captures this.

data Card =
  MkCard CardColour CardRank
  deriving (Eq, Show) -- for Eq see later

-- *** Step 9. The score of a card is the sum of the
-- score of its colour and the score of its rank.
-- Let's define a function for this.

cardScore :: Card -> Int
cardScore (MkCard colour rank) =
  colourScore colour + rankScore rank

-- >>> cardScore (MkCard Red Rank2)
-- 22

-- *** Step 10. Sometimes we also want to access the
-- colour and the rank of a given card. We can always
-- use pattern matching, but we can also define dedicated
-- selector functions.

cardColour :: Card -> CardColour
cardColour (MkCard colour _rank) =
  colour

cardRank :: Card -> CardRank
cardRank (MkCard _colour rank) =
  rank

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

-- >>> compactCard (MkCard Red Rank2)
-- "R-2"

-- *** Step 12. A "combination" is either a pair or a triple
-- of cards. Let's define a datatype for this.

data Combination =
    Pair Card Card
  | Triple Card Card Card
  deriving (Eq, Show) -- for Eq see later

-- *** Step 13. Combinations also have scores.
-- The score of a combination
-- is the sum of the cards involved, except if all of them
-- have the same colour: then the original score is doubled.
-- We can first define a helper function for the plain score,
-- which is always the sum.

plainScore :: Combination -> Int
plainScore (Pair c1 c2)      =
  cardScore c1 + cardScore c2
plainScore (Triple c1 c2 c3) =
  cardScore c1 + cardScore c2 + cardScore c3

-- *** Step 14. We need a way to check if two cards are
-- of the same colour. We can define this by pattern
-- matching as well.

equalColour :: CardColour -> CardColour -> Bool
equalColour Yellow Yellow = True
equalColour Red    Red    = True
equalColour Purple Purple = True
equalColour Black  Black  = True
equalColour _      _      = False

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
sameColourCombination (Pair c1 c2) =
  cardColour c1 == cardColour c2
sameColourCombination (Triple c1 c2 c3) =
  cardColour c1 == cardColour c2
  && cardColour c2 == cardColour c3

combinationScore :: Combination -> Int
combinationScore comb =
  if sameColourCombination comb
    then plainScore comb * 2
    else plainScore comb

-- *** Step 17. A stack of cards is a (possibly)
-- empty number of cards placed on top of one another.
--
-- One way to model this is:
-- - a stack can be empty;
-- - if a stack isn't empty, there's a top card and a (smaller)
--   stack below.
--
data Stack =
    Empty
  | OnTop Card Stack

-- *** Step 18. Let's define a function that counts
-- the number of cards in a stack.

stackSize :: Stack -> Int
stackSize Empty           = 0
stackSize (OnTop _ stack) = 1 + stackSize stack

-- Interlude: standard design pattern

-- *** Step 19. Sometimes, we want to remove all cards
-- of a given colour from a stack of cards, keeping
-- the others in their original order. Let's
-- write a function for this.

removeColour :: CardColour -> Stack -> Stack
removeColour _colour Empty              = Empty
removeColour colour (OnTop card stack)
  | cardColour card == colour          = removeColour colour stack
  | otherwise                          = OnTop card (removeColour colour stack)

-- *** Step 20. Sometimes, we want to flip a stack,
-- reversing the order of all the cards.
-- (accumulator, let/where, partial parameterisation)

flipStack :: Stack -> Stack
flipStack =
  flipStackAux Empty
  where
    flipStackAux :: Stack -> Stack -> Stack
    flipStackAux acc Empty           = acc
    flipStackAux acc (OnTop c stack) =
      flipStackAux (OnTop c acc) stack

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

highestCard :: Stack -> Maybe Card
highestCard Empty              = Nothing
highestCard (OnTop card stack) = Just (highestCardAux card stack)
  where
    highestCardAux :: Card -> Stack -> Card
    highestCardAux acc Empty               = acc
    highestCardAux acc (OnTop card' stack')
      | cardScore card' > cardScore acc    = highestCardAux card' stack'
      | otherwise                          = highestCardAux acc   stack'

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

stackToList :: Stack -> [Card]
stackToList Empty              = []
stackToList (OnTop card cards) = card : stackToList cards

listToStack :: [Card] -> Stack
listToStack []             = Empty
listToStack (card : cards) = OnTop card (listToStack cards)

-- *** Step 23. Let's directly redefine flipStack on lists.
-- It doesn't make any assumptions about the type of elements,
-- so it can be (parametrically) polymorphic.

reverseList :: [a] -> [a]
reverseList = reverseListAux []
  where
    reverseListAux :: [a] -> [a] -> [a]
    reverseListAux acc []       = acc
    reverseListAux acc (x : xs) = reverseListAux (x : acc) xs

-- This function exists in the library as 'reverse'.

-- *** Step 24. Let's define a function that appends two lists.

appendList :: [a] -> [a] -> [a]
appendList []       other = other
appendList (x : xs) other = x : appendList xs other

-- This function exists in the library as '++'.

-- *** Step 25. The function that removes all cards of a given
-- colour can be generalised by abstracting over a predicate
-- on the elements. This is our first true higher-order function!

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ []       = []
filterList p (x : xs)
  | p x               = x : filterList p xs
  | otherwise         = filterList p xs

-- This function exists in the library as 'filter'.

-- *** Step 26. Let's now actually re-express 'removeColour'
-- via 'filterList' (but on '[Card]' rather than 'Stack').
-- (lambda expressions)

removeColour' :: CardColour -> [Card] -> [Card]
removeColour' colour = filterList (\ card -> cardColour card == colour)

-- *** Step 27. Every card has a matching card. The matching
-- card is the one of the same colour so that the ranks add up
-- to 4. Let's define a function that computes the matching card
-- for a given card.

matchingRank :: CardRank -> CardRank
matchingRank Rank1 = Rank3
matchingRank Rank2 = Rank2
matchingRank Rank3 = Rank1

matchingCard :: Card -> Card
matchingCard (MkCard colour rank) = MkCard colour (matchingRank rank)

-- *** Step 28. For a stack of cards, let's compute the stack of
-- cards where every card has been replaced by its matching card.

matchingStack :: Stack -> Stack
matchingStack Empty              = Empty
matchingStack (OnTop card stack) = OnTop (matchingCard card) stack

-- *** Step 29. The idea to apply the same function to every element
-- in a list is very common. Let's define it in general on lists,
-- abstracting over the function to be applied.

mapList :: (a -> b) -> [a] -> [b]
mapList _ []       = []
mapList f (x : xs) = f x : mapList f xs

matchingCardList :: [Card] -> [Card]
matchingCardList = mapList matchingCard

-- *** Step 30. We want to define a list enumerating all possible cards.
-- Let's start by enumerating all possible colours and ranks.

allColours :: [CardColour]
allColours = [Yellow, Red, Purple, Black]

-- (For simple enumeration types like this, but not for more complex
-- types, this can also be achieved by deriving the 'Bounded' and 'Enum'
-- type classes).

allRanks :: [CardRank]
allRanks = [minBound .. maxBound]

-- *** Step 31. Now we can use 'map' over these lists to
-- get all the cards as a list of lists.

cardsByColour :: [[Card]]
cardsByColour =
  map (\ colour -> map (\ rank -> MkCard colour rank) allRanks) allColours

-- >>> cardsByColour
-- [[MkCard Yellow Rank1,MkCard Yellow Rank2,MkCard Yellow Rank3],[MkCard Red Rank1,MkCard Red Rank2,MkCard Red Rank3],[MkCard Purple Rank1,MkCard Purple Rank2,MkCard Purple Rank3],[MkCard Black Rank1,MkCard Black Rank2,MkCard Black Rank3]]

-- *** Steps 32. Let's try to flatten such a list of lists into a
-- single list.

flattenList :: [[a]] -> [a]
flattenList []         = []
flattenList (xs : xss) = xs ++ flattenList xss

-- This function exists in the library as 'concat'.

-- *** Step 33. Now we can really get all the cards.

allCards :: [Card]
allCards =
  concat cardsByColour

-- >>> allCards
-- [MkCard Yellow Rank1,MkCard Yellow Rank2,MkCard Yellow Rank3,MkCard Red Rank1,MkCard Red Rank2,MkCard Red Rank3,MkCard Purple Rank1,MkCard Purple Rank2,MkCard Purple Rank3,MkCard Black Rank1,MkCard Black Rank2,MkCard Black Rank3]

-- *** Step 34. What if we want the compact names for all these cards?
-- What if we want the scores of all these cards?

cardNames :: [String]
cardNames =
  map compactCard allCards

cardScores :: [Int]
cardScores =
  map cardScore allCards

-- >>> cardNames
-- ["Y-1","Y-2","Y-3","R-1","R-2","R-3","P-1","P-2","P-3","B-1","B-2","B-3"]
--
-- >>> cardScores
-- [11,12,13,21,22,23,31,32,33,101,102,103]

-- *** Step 35. Let's define a function on lists that checks whether
-- a particular element occurs in the list.
-- (There are many ways to achieve this now.)

elemList :: Eq a => a -> [a] -> Bool
elemList _ []       = False
elemList x (y : ys) = x == y || elem x ys

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

allPairCombinations :: [Combination]
allPairCombinations =
  concat (map (\ card1 -> map (\ card2 -> Pair card1 card2) allCards) allCards)

allTripleCombinations :: [Combination]
allTripleCombinations = do
  card1 <- allCards
  card2 <- allCards
  card3 <- allCards
  pure (Triple card1 card2 card3)

allCombinations :: [Combination]
allCombinations =
  allPairCombinations ++ allTripleCombinations

allCombinationScores :: [Int]
allCombinationScores =
  map combinationScore allCombinations

combinationScoreExists :: Int -> Bool
combinationScoreExists score =
  elemList score allCombinationScores

-- >>> Data.List.sort (Data.List.nub (map combinationScore allTripleCombinations))
-- [43,44,45,46,47,48,49,53,54,55,56,57,58,59,63,64,65,66,67,68,69,70,72,73,74,75,76,77,78,79,83,84,85,86,87,88,89,123,124,125,126,127,128,129,130,132,133,134,135,136,137,138,139,143,144,145,146,147,148,149,153,154,155,156,157,158,159,163,164,165,166,167,168,169,186,188,190,192,194,196,198,213,214,215,216,217,218,219,223,224,225,226,227,228,229,233,234,235,236,237,238,239,606,608,610,612,614,616,618]


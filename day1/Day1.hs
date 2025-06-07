-- Copyright (c) 2025, Andres Löh, Well-Typed LLP
module Day1 where

-- Let's say we want to model aspects of a card game.
-- Cards have colours (yellow, red, purple, or black)
-- and a rank which is one, two or three.

-- *** Step 1. Let's define a datatype for card colours.

-- *** Step 2. To work with card colours conveniently,
-- it is helpful if we can evaluate expressions that compute
-- colours and print the results to screen. In order to do
-- that, GHC has to know what string to associate with
-- each of the data constructors of the type.
--
-- One way to do this is to define a function for this ...

-- *** Step 3. And then to instantiate the 'Show' class
-- for 'CardColour'. This means we can use the name 'show'
-- to invoke 'showCardColour', but we can also use the same
-- name 'show' to turn other types into strings.
--
-- And GHC can now use other functions that are internally
-- using 'show' also on the type 'CardColour'.

-- *** Step 4. Let's see if we can evaluate values of this
-- type and ask for their inferred types. We can use the
-- eval plugin or GHCi for this.

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

-- *** Step 6. Colours and ranks are associated with a
-- point score. Let's start with colours:
-- Yellow is worth 10 points, red 20, purple 30 and black 100.
-- Let's define a function that captures this.
--
-- Note that a type signature is optional, but strongly
-- recommended.

-- *** Step 7. Ranks also map to scores. This
-- is the obvious mapping, e.g. rank 2 is associated with
-- 2 points. Let's define a function for this.

-- *** Step 8. Actual cards have a colour and a rank.
-- Let's define a datatype that captures this.

-- *** Step 9. The score of a card is the sum of the
-- score of its colour and the score of its rank.
-- Let's define a function for this.

-- *** Step 10. Sometimes we also want to access the
-- colour and the rank of a given card. We can always
-- use pattern matching, but we can also define dedicated
-- selector functions.

-- This can more easily be done with records.

-- *** Step 11. Let's define a more compact textual
-- representation for cards. It's tempting but usually not
-- advisable to use the 'Show' class for this. So we just
-- define separate functions for colours and cards that abbreviates
-- colours by single letters and uses a dash to combine a
-- colour with a rank. For ranks, we could define another
-- function as well, but we can also use 'show' on the score
-- of the rank.

-- *** Step 12. A "combination" is either a pair or a triple
-- of cards. Let's define a datatype for this.

-- *** Step 13. Combinations also have scores.
-- The score of a combination
-- is the sum of the cards involved, except if all of them
-- have the same colour: then the original score is doubled.
-- We can first define a helper function for the plain score,
-- which is always the sum.

-- *** Step 14. We need a way to check if two cards are
-- of the same colour. We can define this by pattern
-- matching as well.

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

-- *** Step 17. A stack of cards is a (possibly)
-- empty number of cards placed on top of one another.
--
-- One way to model this is:
-- - a stack can be empty;
-- - if a stack isn't empty, there's a top card and a (smaller)
--   stack below.
--

-- *** Step 18. Let's define a function that counts
-- the number of cards in a stack.

-- Interlude: standard design pattern

-- *** Step 19. Sometimes, we want to remove all cards
-- of a given colour from a stack of cards, keeping
-- the others in their original order. Let's
-- write a function for this.

-- *** Step 20. Sometimes, we want to flip a stack,
-- reversing the order of all the cards.
-- (accumulator, let/where, partial parameterisation)

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


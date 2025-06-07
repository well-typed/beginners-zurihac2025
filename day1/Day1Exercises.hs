-- Copyright (c) 2025, Andres Löh, Well-Typed LLP
module Day1Exercises where

-- This file contains additional exercises that should be doable
-- with roughly the material we've covered on Day1.
--
-- Yet more possible exercises are in "Assignments A" and
-- "Assignments B" of Part 2 of the Well-Typed Haskell intro course:
--
-- https://teaching.well-typed.com/intro/datatypes-and-functions.html
--
-- Note that some of these may be using stuff we haven't explicitly
-- covered though, and others may just be duplicates or near-duplicates
-- of stuff we've already gone through or that is in this file.

-- *** PART A: Boxes

-- *** Exercise A1.
--
-- A box has a width and a height, both of which are integers.
-- (Our boxes are two-dimensional). Define a function that
-- computes the area of a box.

data Box =
  MkBox { boxWidth :: Int, boxHeight :: Int }
  deriving (Eq, Show)

-- *** Exercise A2.
--
-- Given a list of boxes, find the one with minimum
-- area (return a 'Maybe').
--
-- For this, follow what we did for 'highestCard'.
--
-- If you know more already or want to get started
-- on using libraries, see if you can figure out how
-- to do this by using 'minimumBy' from 'Data.List'.

-- *** Exercise A3.
--
-- A box arrangement is an arrangement of zero or more
-- boxes. A box arrangement can be:
--
-- - empty
-- - contain a single box
-- - consist of two arrangements placed next to each other (horizontally)
-- - consist of two arrangements places on top of each other (vertically)
--
-- Box arrangements can effectively contain gaps. In the
-- following, we are not assuming that any forces such as
-- gravity apply to these boxes.
--
-- Define a datatype for such arrangements.

-- *** Exercise A4.
--
-- Compute the total width of a box arrangement:
--
-- - the total width of the empty arrangement is 0
-- - the total width of two horizontally connected arrangegements is
--   the sum of the widths of the individual arrangements
-- - the total width of two vertically connected arrangements is the
--   maximum of the widths of the individual arrangements.
--
-- Compute the total height of an arrangement correspondingly.
--
-- Compute the total area needed for an arrangement.

-- *** Exercise A5.
--
-- Compute all possible arrangements for a given list of boxes
-- that keep their relative order.
--
-- So e.g. for two boxes b1 and b2, you can place b1 horizontally
-- next to b2 or b1 vertically next to b2, but you don't also have
-- to consider placing b2 horizontally next to b1 ...
--
-- Note that an empty list has a possible arrangement.

-- *** Exercise A6.
--
-- Given a list of boxes, find the arrangement that minimises
-- the total area.

-- *** PART 2: Cave systems (binary trees)

-- *** Exercise B1.
-- A plain cave system is structured as follows. Tunnels lead either:
-- - to a dead end
-- - to an exit to the outside world
-- - to a fork where you can choose to follow a left or a right tunnel.
--
-- Define a datatype 'PlainCave' for this.

-- *** Exercise B2.
-- Define a function that counts the number of exists in a given
-- cave system.

-- *** Exercise B3.
-- Define a function that finds the lengths of all paths leading
-- to an exit.

-- *** Exercise B4.
-- Define a function function that determines the number of "steps"
-- needed to the nearest exit. Note that there might not be any exits,
-- so define an appropriate type signature.

-- *** Exercise B5.
-- Define a function that generates a balanced cave system where every
-- path ends in an exit (i.e., there are no dead ends), and every
-- path has the given length.

-- *** Exercise B6.
-- Define a property that should evaluate to True for all numbers,
-- relating the generation function from task B5 with the path
-- length function from task B3.

-- *** Exercise B7.
-- A path is a list of directions. You can define a type synonym
-- (alias) for paths like this:
--
-- type Path = [Direction]
--
-- Then 'Path' and '[Direction]' can be used interchangeably.
-- Define a datatype 'Direction' so that you have two options:
-- 
-- - to go left
-- - to go right
--
-- (Do *not* call the constructors literally 'Left' or 'Right',
-- because these are already used for the 'Either' type and this
-- may lead to weird name clashes.)

-- *** Exercise B8.
-- Define a function that takes a path and a cave and determines
-- the "rest" of the cave system at the end of the path. If
-- the path goes on past an exit or a dead end, return 'Nothing'.

-- *** Exercise B9.
-- Define a function that determines the paths to all the exits
-- in a given cave system.

-- *** Exercise B10.
-- Define a property that should evaluate to True for all cave
-- systems, relating the path finding function of task B9 with
-- the path following function from task B8.

-- *** Exercise B11.
-- Define a function that takes a cave system and a path and
-- it will replace the point at the end of the path with a
-- dead end. If the path does not lead anywhere in the current
-- cave system already, the original cave system should be
-- returned.

-- *** Exercise B12.
-- Define a function that takes a cave system and a list of
-- paths and iteratively replaces subcaves with dead ends using
-- the function from task B11.
--
-- Use this function and the generation function from task B5
-- to define a concrete somewhat interesting cave system.

-- *** PART 3: Complex cave systems (depends on aspects of PART 2)

-- *** Exercise C1.
-- A complex cave system is a cave system where every dead end
-- actually contains an "item" of a flexible type. I.e., define
-- a type for complex cave system that is parameterised.

-- *** Exercise C2.
-- There's the unit type
--
-- data () = ()
--
-- (a type with a single constructor also called ()). A complex
-- cave system instantiated to '()' should be in direct correspondence
-- with a plain cave system. Write conversion functions back and
-- forth.
--
-- *** Exercise C3.
-- Define a function
--
--   distribute :: [a] -> ComplexCave b -> ComplexCave (Maybe a)
--
-- that places the items in the list into the dead ends in the
-- given cave system from left to right. Superfluous items are
-- ignored. If there aren't enough items, place 'Nothing'.
--
-- *** Exercise C4.
-- Define a function
--
--   collect :: ComplexCave a -> [a]
--
-- that collects all the items from a cave system in order.
--
-- *** Exercise C5.
-- Define a property that should evaluate to True for any combination
-- of a complex cave system and a list of items that relates
-- 'distribute' from task C3 and 'collect' from task C4.
--
-- *** Exercise C6.
-- Define a map function for complex cave systems that modifies
-- every item using the given function:
--
--   mapCave :: (a -> b) -> ComplexCave a -> ComplexCave b
--
-- *** Exercise C7.
-- An interdimensional cave system is a cave system where at every
-- dead end, there is an interdimensional portal that leads to
-- another cave system. Define a function
--
--   collapse :: ComplexCave (ComplexCave a) -> ComplexCave a
--
-- that invokes an incantation that makes the portals imperceptible.


module Day2 where

import System.Random
import Text.Read

-- Plan:
--
-- Random numbers
-- Write a simple interactive guessing game

-- >>> :t randomRIO
-- randomRIO :: (Random a, MonadIO m) => (a, a) -> m a
-- randomRIO :: (Int, Int) -> IO Int

roll :: IO Int
roll = randomRIO (1, 6)

-- >>> roll
-- 6

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- pure :: a -> IO a

rollTwiceAndSum :: IO Int
rollTwiceAndSum =
  roll >>= \ r1 ->
  roll >>= \ r2 ->
  pure (r1 + r2)

rollTwiceAndSum' :: IO Int
rollTwiceAndSum' = do
  r1 <- roll
  r2 <- roll
  pure (r1 + r2)

-- >>> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- >>> :t return
-- return :: Monad m => a -> m a
-- >>> :t pure
-- pure :: Applicative f => a -> f a

-- >>> rollTwiceAndSum
-- 3

rollOnceAndDouble :: IO Int
rollOnceAndDouble =
  roll >>= \ r -> pure (r + r)

-- >>> rollOnceAndDouble
-- 12

data Stuff = MkStuff Int Int
  deriving Show

rollThriceAndDoStuff :: IO Stuff
rollThriceAndDoStuff = do
  r1 <- roll
  r2 <- roll
  r3 <- roll
  pure (MkStuff (r1 + r2) (r2 * r3))

-- times1 :: Int -> IO a -> [IO a]
myreplicate :: Int -> a -> [a]
myreplicate i action
  | i <= 0    = []
  | otherwise = action : myreplicate (i - 1) action

-- >>> replicate 5 3
-- [3,3,3,3,3]
-- >>> times1 10 'x'
-- "xxxxxxxxxx"

mysequence :: [IO a] -> IO [a]
mysequence [] = pure []
mysequence (action : actions) = do
  r <- action
  rs <- mysequence actions
  pure (r : rs)

myreplicateM :: Int -> IO a -> IO [a]
myreplicateM i action = sequence (replicate i action)

-- >>> myreplicateM 20 rollThriceAndDoStuff
-- [MkStuff 6 16,MkStuff 7 18,MkStuff 8 16,MkStuff 4 15,MkStuff 8 6,MkStuff 5 12,MkStuff 11 6,MkStuff 5 4,MkStuff 5 18,MkStuff 7 24,MkStuff 8 4,MkStuff 8 4,MkStuff 3 5,MkStuff 5 3,MkStuff 9 30,MkStuff 6 12,MkStuff 10 25,MkStuff 6 12,MkStuff 9 30,MkStuff 8 18]

mymapM :: (a -> IO b) -> [a] -> IO [b]
mymapM f xs = sequence (map f xs)

-- >>> mymapM randomRIO [(1,5), (2,6), (4,22), (1000, 2000)]
-- [5,2,22,1722]

-- >>> :t putStrLn
-- putStrLn :: String -> IO ()
-- >>> :t getLine
-- getLine :: IO String

-- Number guessing game

data GameState =
  MkGameState
    { gameTurn   :: Int
    , gameSecret :: Int
    }
  deriving Show

initialGameState :: Int -> GameState
initialGameState secret =
  MkGameState
    { gameTurn   = 1
    , gameSecret = secret
    }

-- A safe getLine function for numbers:
readNumber :: String -> IO Int
readNumber prompt = do
  putStrLn prompt
  line <- getLine
  case readMaybe line of
    Nothing -> do
      -- input could not be parsed as a number; complain and try again
      putStrLn "Could not parse number."
      readNumber prompt
    Just i  -> pure i

game :: GameState -> IO ()
game (MkGameState turn secret) = do
  guess <- readNumber ("Turn " ++ show turn ++ ". Please enter a number:")
  -- compare returns type Ordering, which is
  -- data Ordering = LT | EQ | GT
  case compare guess secret of
    LT -> do -- note: do has to be repeated if constructs such as case intervene
             -- and we want to sequence several actions
      putStrLn "Your guess was too small."
      game (MkGameState (turn + 1) secret)
    EQ ->
      putStrLn "You win!"
    GT -> do
      putStrLn "Your guess was too large."
      game (MkGameState (turn + 1) secret)

randomGame :: IO ()
randomGame = do
  secret <- randomRIO (1, 100)
  game (initialGameState secret)

-- You have to execute readNumber, game and randomGame in ghci;
-- they won't work well with the eval plugin because they expect input.

-- Ideas for extension:
--
-- - Limit the number of rounds.
-- - Figure out whether the user makes clear mistakes, i.e., guesses
--   numbers that could already have been ruled out by previous answers.
--   This means you have to track bounds for the secret the user already
--   knows about and update them accordingly.
-- - Make the game more difficult / more interesting.

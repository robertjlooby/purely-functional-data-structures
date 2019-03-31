module RealTimeQueue
  ( Queue
  , empty
  , head
  , isEmpty
  , queue
  , snoc
  , tail
  ) where

import           Data.Foldable ( toList )
import           Prelude hiding ( head, tail )

-- | The real-time queue data structure from Chapter 4 of PFDS. Provides
-- constant time 'head', 'snoc', and 'tail' operations.
data Queue a =
    Queue
        { f :: [a]
        , r :: [a]
        , s :: [a]
        }
    deriving ( Show )

instance Eq a => Eq (Queue a) where
    (Queue f1 r1 _) == (Queue f2 r2 _) =
        f1 ++ (reverse r1) == f2 ++ (reverse r2)

-- | The smart constructor exposed in PFDS takes all three components of the
-- queue. Besides exposing too much of the internals it could allow someone to
-- construct an invalid queue. I've kept that implementation as 'queueInternal'
-- below and only expose a smart constructor taking a single list.
--
-- I've also relaxed the type constraint to only require an instace of 'Foldable'.
queue :: Foldable f => f a -> Queue a
queue xs = Queue f [] f
  where
    f = toList xs

queueInternal :: [a] -> [a] -> [a] -> Queue a
queueInternal f r (_ : s) = Queue f r s
queueInternal f r [] = Queue f' [] f'
  where
    f' = rotate f r []

    rotate [] (y : _) s = y : s
    rotate (x : f) (y : r) s = x : rotate f r (y : s)

empty :: Queue a
empty = Queue [] [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] _ _) = True
isEmpty _ = False

-- | The 'head' in PFDS throws an exception on an empty queue. I chose to
-- change the type signature to return a `Maybe a` instead.
head :: Queue a -> Maybe a
head (Queue [] _ _) = Nothing
head (Queue (a : _) _ _) = Just a

-- | The 'tail' in PFDS throws an exception on an empty queue. I chose to
-- instead make 'tail empty' return another empty queue.
tail :: Queue a -> Queue a
tail (Queue [] _ _) = empty
tail (Queue (_ : f) r s) = queueInternal f r s

snoc :: Queue a -> a -> Queue a
snoc (Queue f r s) x = queueInternal f (x : r) s

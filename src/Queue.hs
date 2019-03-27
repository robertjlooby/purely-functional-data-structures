module Queue
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

-- | The queue data structure from Chapter 3 of PFDS. Provides constant time
-- 'head' and 'snoc' operations as well as an amortized constant time 'tail'
-- operation.
data Queue a =
    Queue
        { f :: [a]
        , r :: [a]
        }
    deriving ( Show )

instance Eq a => Eq (Queue a) where
    (Queue f1 r1) == (Queue f2 r2) = f1 ++ (reverse r1) == f2 ++ (reverse r2)

-- | The smart constructor in PFDS takes both halves of the queue. I think this
-- leaks too much of the internal structure of the queue. A user could
-- reasonably expect 'queue [1, 2] [3, 4]' to be the same as
-- 'queue [1, 2, 3, 4] []' when it's not. At any rate I can't envision a use
-- case outside of testing where it would be desirable to construct the queue
-- with two lists instead of one.
--
-- I've also relaxed the type constraint to only require an instace of 'Foldable'.
queue :: Foldable f => f a -> Queue a
queue = flip Queue [] . toList

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] _) = True
isEmpty _ = False

-- | The 'head' in PFDS throws an exception on an empty queue. I chose to
-- change the type signature to return a `Maybe a` instead.
head :: Queue a -> Maybe a
head (Queue [] _) = Nothing
head (Queue (a : _) _) = Just a

-- | The 'tail' in PFDS throws an exception on an empty queue. I chose to
-- instead make 'tail empty' return another empty queue.
tail :: Queue a -> Queue a
tail (Queue [] _) = empty
tail (Queue [ _ ] r) = Queue (reverse r) []
tail (Queue (_ : xs) ys) = Queue xs ys

snoc :: Queue a -> a -> Queue a
snoc (Queue [] _) a = Queue [ a ] []
snoc (Queue f r) a = Queue f (a : r)

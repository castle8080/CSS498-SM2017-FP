module Queue (
    Queue,
    q_empty,
    q_enqueue,
    q_dequeue,
    q_enqueue_all,
    q_dequeue_all
) where

import Data.Maybe
import Data.List

-- A queue can be implemented with 2 lists.
-- The first list is where incoming elements go.
-- The second list is where elements are pulled from.
type Queue a = ([a], [a])

-- The empty queue.
q_empty :: Queue a
q_empty = ([], [])

-- Adds an element to a queue.
q_enqueue :: a -> (Queue a) -> (Queue a)
q_enqueue x (q_in, q_out) = (x : q_in, q_out)

-- Removes an element from a queue.
q_dequeue :: (Queue a) -> (Maybe a, Queue a)
-- The queue is empty.
q_dequeue ([], []) =
    (Nothing, ([], []))
-- The outgoing list has an element.
q_dequeue (q_in, x : q_out) =
    (Just x, (q_in, q_out))
-- The outoing list is empty but the incoming list is not.
q_dequeue (q_in, []) =
    q_dequeue ([], reverse q_in)

    
q_enqueue_all :: [a] -> (Queue a) -> (Queue a)
q_enqueue_all xs q = foldl (\q x -> q_enqueue x q) q xs

q_dequeue_all :: (Queue a) -> [a]
q_dequeue_all q =
    case q_dequeue q of
        (Nothing, _) -> []
        (Just x, q') -> x : q_dequeue_all q'

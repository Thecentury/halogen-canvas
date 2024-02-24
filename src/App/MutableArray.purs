module App.MutableArray (
  Iterator(..),
  iterator,
  iteratorAt,
  iterateReverseWithIndex,
  prev,
  peekWithIndex
) where

import Prelude
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))

-- | This type provides a slightly easier way of iterating over an array's
-- | elements in an STArray computation, without having to keep track of
-- | indices.
data Iterator r a = Iterator (Int -> ST r (Maybe a)) (STRef r Int)

-- | Make an Iterator given an indexing function into an array (or anything
-- | else). If `xs :: Array a`, the standard way to create an iterator over
-- | `xs` is to use `iterator (xs !! _)`, where `(!!)` comes from `Data.Array`.
iterator :: forall r a. (Int -> ST r (Maybe a)) -> ST r (Iterator r a)
iterator f = Iterator f <$> STRef.new 0

iteratorAt :: forall r a. Int -> (Int -> ST r (Maybe a)) -> ST r (Iterator r a)
iteratorAt i f = Iterator f <$> STRef.new i

iterateReverseWithIndex :: forall r a. Iterator r a -> (a -> ST r Unit) -> ST r Unit
iterateReverseWithIndex iter f = do
  break <- STRef.new false
  ST.while (not <$> STRef.read break) do
    mx <- prev iter
    case mx of
      Just x -> f x
      Nothing -> void $ STRef.write true break

prev :: forall r a. Iterator r a -> ST r (Maybe a)
prev (Iterator f currentIndex) = do
  i <- STRef.read currentIndex
  _ <- STRef.modify (_ - 1) currentIndex
  element <- f i
  pure element

{----------------------------------------------------------------------------------------------------------------------}

peekWithIndex :: forall h a . STArray h a -> Int -> ST h (Maybe (Tuple Int a))
peekWithIndex cells i = do
  cell <- STArray.peek i cells
  pure $ (Tuple i) <$> cell
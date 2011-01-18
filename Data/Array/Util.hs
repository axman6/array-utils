{-# LANGUAGE BangPatterns #-}
module Data.Array.Util (
    ) where

import Data.Array.Base
import Data.Array.MArray
import Data.Ix
import Control.Monad

{-# INLINE mapElems #-}
-- | mapElems mutates every element in an array while avoiding
-- all bounds checks. /O(size of arr)/
mapElems :: (MArray a e m, Ix i) => (e -> e) -> a i e -> m ()
mapElems f arr = do
    (start, end') <- getBounds arr
    let !end = index (start, end') end'
    forM_ [0..end] $ \i ->
        unsafeRead arr i >>= unsafeWrite arr i . f

{-# INLINE mapElemsIx #-}
-- | The same as mapElems, but also providing the index to the
-- mapping function. /O(size of arr)/
mapElemsIx :: (MArray a e m, Ix i) => (i -> e -> e) -> a i e -> m ()
mapElemsIx f arr =do
    (start, end') <- getBounds arr
    let !end = index (start, end') end'
    forM_ (zip [0..end] (range (start, end'))) $ \(i,i') ->
        unsafeRead arr i >>= unsafeWrite arr i . f i'

{-# INLINE mapElems' #-}
-- | Takes a mapping function, and a list of indicies to mutate.
-- Returns True if the muration occured, and False if the indicies
-- are out of range. /O(length xs)/
mapElems' :: (MArray a e m, Ix i) => (e -> e) -> [i] -> a i e -> m Bool
mapElems' f xs arr = do
    (lo, hi) <- getBounds arr
    let toOffset = index (lo, hi)
        ok = not (all (\x -> x >= lo && x <= hi) xs)
    when ok $ forM_ (map toOffset xs) $ \i ->
        unsafeRead arr i >>= unsafeWrite arr i . f
    return ok


{-# INLINE mapElemsIx' #-}
-- | Takes a mapping function, and a list of indicies to mutate.
-- Returns True if the muration occured, and False if the indicies
-- are out of range. /O(length xs)/
mapElemsIx' :: (MArray a e m, Ix i) => (i -> e -> e) -> [i] -> a i e -> m Bool
mapElemsIx' f xs arr = do
    (lo, hi) <- getBounds arr
    let toOffset = index (lo, hi)
        ok = not (all (\x -> x >= lo && x <= hi) xs)
    when ok $ forM_ (zip (map toOffset xs) xs) $ \(i,i') ->
        unsafeRead arr i >>= unsafeWrite arr i . f i'
    return ok

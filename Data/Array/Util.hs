{-# LANGUAGE BangPatterns #-}
module Data.Array.Util
    ( updateElems
    , updateElemsIx
    , updateElems'
    , updateElemsIx'
    ) where

import Data.Array.Base
import Data.Array.MArray
import Control.Monad
import Control.Exception

{-# INLINE updateElems #-}
-- | updateElems mutates every element in an array while avoiding
-- all bounds checks. /O(size of arr)/
updateElems :: (MArray a e m, Ix i) => (e -> e) -> a i e -> m ()
updateElems f arr = do
    (start, end') <- getBounds arr
    let !end = index (start, end') end'
    forM_ [0..end] $ \i -> unsafeRead arr i >>= unsafeWrite arr i . f


{-# INLINE updateElemsIx #-}
-- | The same as updateElems, but also providing the index to the
-- mapping function. /O(size of arr)/
updateElemsIx :: (MArray a e m, Ix i) => (i -> e -> e) -> a i e -> m ()
updateElemsIx f arr =do
    bnds@(start, end') <- getBounds arr
    let !end = index (start, end') end'
        -- Should be guaranteed that xs will be null when i = end+1
        go !i (!x:xs) | i <= end = do
            e <- unsafeRead arr i
            unsafeWrite arr i $ f x e
            go (i+1) xs
        go _ _ = return ()
    go 0 (range bnds)


{-# INLINE updateElemsWithin #-}
-- | Takes an update function `f` and a tuple of indicies `(start, finish)`
-- , and applies the function to all elements returned by range (start, finish).
-- Throws an IndexOutOfBounds exception if either of the indicies are out of bounds.
updateElemsWithin :: (MArray a e m, Ix i) => (e -> e) -> (i,i) -> a i e -> m ()
updateElemsWithin f (start, finish) arr = do
    bnds <- getBounds arr
    let ok = inRange bnds start && inRange bnds finish
        indicies = map (index bnds) $ range (start, finish)
    when (not ok) $ throw $ IndexOutOfBounds $ "Data.Array.Util updateElemsWithin"
    forM_ indicies $ \i -> unsafeRead arr i >>= unsafeWrite arr i . f


{-# INLINE updateElemsWithinIx #-}
-- | Takes an update function `f` and a tuple of indicies `(start, finish)`
-- , and applies the function to all elements returned by range (start, finish).
-- Throws an IndexOutOfBounds exception if either of the indicies are out of bounds.
updateElemsWithinIx :: (MArray a e m, Ix i) => (i -> e -> e) -> (i,i) -> a i e -> m ()
updateElemsWithinIx f (start, finish) arr = do
    bnds <- getBounds arr
    let ok = inRange bnds start && inRange bnds finish
        rnge = range (start, finish)
        indicies = map (index bnds) rnge
        go (!i:is) (!x:xs) = do
            e <- unsafeRead arr i
            unsafeWrite arr i $ f x e
        go _ _ = return ()
    when (not ok) $ throw $ IndexOutOfBounds $ "Data.Array.Util updateElemsWithin"
    go indicies rnge


{-# INLINE updateElems' #-}
-- | Takes a mapping function, and a list of indicies to mutate.
-- Throws an IndexOutOfBounds exception if any of the indicies are
-- out of bounds. In this case the array will be left unmutated.
-- /O(length xs)/
updateElems' :: (MArray a e m, Ix i) => (e -> e) -> [i] -> a i e -> m ()
updateElems' f xs arr = do
    bnds <- getBounds arr
    let !ok = all (inRange bnds) xs
        toOffset = index bnds
    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElems'")
    forM_ (map toOffset xs) $ \i -> unsafeRead arr i >>= unsafeWrite arr i . f


{-# INLINE updateElemsIx' #-}
-- | Takes a mapping function which takes an index, and a list of indicies
-- to mutate. Throws IndexOutOfBounds exception as `updateElems'` does.
-- /O(length xs)/
updateElemsIx' :: (MArray a e m, Ix i) => (i -> e -> e) -> [i] -> a i e -> m ()
updateElemsIx' f xs arr = do
    bnds@(lo, hi) <- getBounds arr
    let !ok =  all (inRange bnds) xs
        toOffset = index (lo, hi)
        ixs = map toOffset xs
        go (!i:is) (!x:xs) = do
            e <- unsafeRead arr i
            unsafeWrite arr i $ f x e
            go is xs
        go _ _ = return ()
    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElemsIx'")
    go ixs xs


{-# LANGUAGE BangPatterns, CPP #-}
module Data.Array.Util
    ( updateElems
    , updateElemsIx
    , updateElemsOn
    , updateElemsIxOn
    , updateElemsWithin
    , updateElemsWithinIx
    , updateElemsM
    , updateElemsIxM
    , updateElemsOnM
    , updateElemsIxOnM
    , updateElemsWithinM
    , updateElemsWithinIxM
    ) where

import GHC.Arr
import Data.Array.Base
import Control.Monad
import Control.Exception

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 700
#define INLINE(x) {-# INLINABLE x #-}
#else -- 700
#define INLINE(x) {-# INLINE x #-}
#endif -- 700
#else
#define INLINE(x) {- not inlined -}
#endif -- __GLASGOW_HASKELL__

{-# INLINE update, #-}
{-# INLINE updateIx #-}
{-# INLINE updateM #-}
{-# INLINE updateIxM #-}

update :: (MArray a e m, Ix i) => a i e -> (e -> e) -> Int -> m ()
update arr f i = unsafeRead arr i >>= unsafeWrite arr i . f

updateIx :: (MArray a e m, Ix i) => a i e -> (i -> e -> e) -> Int -> i -> m ()
updateIx arr f i x = unsafeRead arr i >>= unsafeWrite arr i . f x

updateM :: (MArray a e m, Ix i) => a i e -> (e -> m e) -> Int -> m ()
updateM arr f i = unsafeRead arr i >>= f >>= unsafeWrite arr i

updateIxM :: (MArray a e m, Ix i) => a i e -> (i -> e -> m e) -> Int -> i -> m ()
updateIxM arr f i x = unsafeRead arr i >>= f x >>= unsafeWrite arr i


INLINE(updateElems)
-- | updateElems mutates every element in an array while avoiding
-- all bounds checks. /O(size of arr)/
updateElems :: (MArray a e m, Ix i) => (e -> e) -> a i e -> m ()
updateElems f arr = do
    bnds@(_ , end') <- getBounds arr
    let !end = unsafeIndex bnds end'
    forM_ [0..end] (update arr f)


INLINE(updateElemsM)
-- | updateElems mutates every element in an array while avoiding
-- all bounds checks. /O(size of arr)/
updateElemsM :: (MArray a e m, Ix i) => (e -> m e) -> a i e -> m ()
updateElemsM f arr = do
    bnds@(_ , end') <- getBounds arr
    let !end = unsafeIndex bnds end'
    forM_ [0..end] (updateM arr f)


INLINE(updateElemsIx)
-- | The same as updateElems, but also providing the index to the
-- mapping function. /O(size of arr)/
updateElemsIx :: (MArray a e m, Ix i) => (i -> e -> e) -> a i e -> m ()
updateElemsIx f arr = do
    bnds@(_ , end') <- getBounds arr
    let !end                    = unsafeIndex bnds end'

        -- Should be guaranteed that xs will be null when i = end+1
        go !i (x:xs) | i <= end = updateIx arr f i x >> go (i+1) xs
        go _ _                  = return ()
    go 0 (range bnds)


INLINE(updateElemsIxM)
-- | The same as updateElems, but also providing the index to the
-- mapping function. /O(size of arr)/
updateElemsIxM :: (MArray a e m, Ix i) => (i -> e -> m e) -> a i e -> m ()
updateElemsIxM f arr = do
    bnds@(_ , end') <- getBounds arr
    let !end                     = unsafeIndex bnds end'

        -- Should be guaranteed that xs will be null when i = end+1
        go !i (x:xs) | i <= end = updateIxM arr f i x >> go (i+1) xs
        go _ _                  = return ()
    go 0 (range bnds)

INLINE(updateElemsWithin)
-- | Takes an update function `f` and a tuple of indicies `(start, finish)`
-- , and applies the function to all elements returned by range (start, finish).
-- Throws an IndexOutOfBounds exception if either of the indicies are out of bounds.
updateElemsWithin :: (MArray a e m, Ix i) => (e -> e) -> (i,i) -> a i e -> m ()
updateElemsWithin f (start, finish) arr = do
    bnds <- getBounds arr
    let !ok      = inRange bnds start && inRange bnds finish
        indicies = map (unsafeIndex bnds) $ range (start, finish)

    when (not ok) $ throw $ IndexOutOfBounds $ "Data.Array.Util updateElemsWithin"
    forM_ indicies (update arr f)


INLINE(updateElemsWithinM)
-- | Takes an update function `f` and a tuple of indicies `(start, finish)`
-- , and applies the function to all elements returned by range (start, finish).
-- Throws an IndexOutOfBounds exception if either of the indicies are out of bounds.
updateElemsWithinM :: (MArray a e m, Ix i) => (e -> m e) -> (i,i) -> a i e -> m ()
updateElemsWithinM f (start, finish) arr = do
    bnds <- getBounds arr
    let !ok      = inRange bnds start && inRange bnds finish
        indicies = map (unsafeIndex bnds) $ range (start, finish)

    when (not ok) $ throw $ IndexOutOfBounds $ "Data.Array.Util updateElemsWithin"
    forM_ indicies (updateM arr f)


INLINE(updateElemsWithinIx)
-- | Takes an update function `f` and a tuple of indicies `(start, finish)`
-- , and applies the function to all elements returned by range (start, finish).
-- Throws an IndexOutOfBounds exception if either of the indicies are out of bounds.
updateElemsWithinIx :: (MArray a e m, Ix i, Show i) => (i -> e -> e) -> (i,i) -> a i e -> m ()
updateElemsWithinIx f (start, finish) arr = do
    bnds <- getBounds arr
    let !ok                = inRange bnds start && inRange bnds finish
        rnge              = range (start, finish)
        indicies          = map (unsafeIndex bnds) rnge

        go (!i:is) (x:xs) = updateIx arr f i x >> go is xs
        go _ _            = return ()

    when (not ok) $ throw $ IndexOutOfBounds $ "Data.Array.Util updateElemsWithinIx"
    go indicies rnge


INLINE(updateElemsWithinIxM)
-- | Takes an update function `f` and a tuple of indicies `(start, finish)`
-- , and applies the function to all elements returned by range (start, finish).
-- Throws an IndexOutOfBounds exception if either of the indicies are out of bounds.
updateElemsWithinIxM :: (MArray a e m, Ix i, Show i) => (i -> e -> m e) -> (i,i) -> a i e -> m ()
updateElemsWithinIxM f (start, finish) arr = do
    bnds <- getBounds arr
    let !ok                = inRange bnds start && inRange bnds finish
        rnge              = range (start, finish)
        indicies          = map (unsafeIndex bnds) rnge

        go (!i:is) (x:xs) = updateIxM arr f i x >> go is xs
        go _ _            = return ()

    when (not ok) $ throw $ IndexOutOfBounds $ "Data.Array.Util updateElemsWithinIx"
    go indicies rnge


INLINE(updateElemsOn)
-- | Takes a mapping function, and a list of indicies to mutate.
-- Throws an IndexOutOfBounds exception if any of the indicies are
-- out of bounds. In this case the array will be left unmutated.
-- /O(length xs)/
updateElemsOn :: (MArray a e m, Ix i) => (e -> e) -> [i] -> a i e -> m ()
updateElemsOn f xs arr = do
    bnds <- getBounds arr
    let !ok      = all (inRange bnds) xs
        toOffset = unsafeIndex bnds

    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElems'")
    forM_ (map toOffset xs) (update arr f)


INLINE(updateElemsOnM)
-- | Takes a mapping function, and a list of indicies to mutate.
-- Throws an IndexOutOfBounds exception if any of the indicies are
-- out of bounds. In this case the array will be left unmutated.
-- /O(length xs)/
updateElemsOnM :: (MArray a e m, Ix i) => (e -> m e) -> [i] -> a i e -> m ()
updateElemsOnM f xs arr = do
    bnds <- getBounds arr
    let !ok      = all (inRange bnds) xs
        toOffset = unsafeIndex bnds

    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElems'")
    forM_ (map toOffset xs) (updateM arr f)


INLINE(updateElemsIxOn)
-- | Takes a mapping function which takes an index, and a list of indicies
-- to mutate. Throws IndexOutOfBounds exception as `updateElems'` does.
-- /O(length xs)/
updateElemsIxOn :: (MArray a e m, Ix i) => (i -> e -> e) -> [i] -> a i e -> m ()
updateElemsIxOn f indexes arr = do
    bnds@(lo, hi) <- getBounds arr
    let toOffset           = unsafeIndex (lo, hi)
        ixs                = map toOffset indexes
        !ok                = all (\x -> x >= 0 && x <= end) ixs

        go (!i:is) (x:xs)  = updateIx arr f i x >> go is xs
        go _ _             = return ()

    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElemsIx'")
    go ixs indexes


INLINE(updateElemsIxOnM)
-- | Takes a mapping function which takes an index, and a list of indicies
-- to mutate. Throws IndexOutOfBounds exception as `updateElems'` does.
-- /O(length xs)/
updateElemsIxOnM :: (MArray a e m, Ix i) => (i -> e -> m e) -> [i] -> a i e -> m ()
updateElemsIxOnM f indexes arr = do
    bnds@(_ , hi) <- getBounds arr
    let toOffset           = unsafeIndex bnds
        !end               = toOffset hi
        ixs                = map toOffset indexes
        !ok                = all (\x -> x >= 0 && x <= end) ixs

        go (!i:is) (x:xs)  = updateIxM arr f i x >> go is xs
        go _ _             = return ()

    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElemsIx'")
    go ixs indexes



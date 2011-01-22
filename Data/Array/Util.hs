{-# LANGUAGE BangPatterns, CPP #-}
module Data.Array.Util
    ( 
    -- $intro

    -- * Updating all elements
      updateElems
    , updateElemsIx
    -- ** Monadic versions
    , updateElemsM
    , updateElemsIxM
    -- * Updating certain elements
    , updateElemsOn
    , updateElemsIxOn
    -- ** Monadic versions
    , updateElemsOnM
    , updateElemsIxOnM
    -- * Updating within a bounded area
    , updateElemsWithin
    , updateElemsWithinIx
    -- ** Monadic versions
    , updateElemsWithinM
    , updateElemsWithinIxM
    -- * Updating slices
    -- $Slice
    , updateElemsSlice
    -- ** Monadic versions
    , updateElemsSliceM
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

-- $intro
-- This module contains some primitive operations for working with
-- mutable 'Arrays'. They all try to avoid bounds checking as much
-- as possible, and should be quite fast.
-- 
-- Some functions throw 'IndexOutOfBounds' exceptions. If an exception
-- is thrown, the array will be left untouched.
-- 
-- This library relies on some of the primitives in "GHC.Arr", so is
-- probably not portable.


-- ================
-- = updateElems* =
-- ================

{-|
updateElems mutates every element in an array while avoiding all bounds checks. /O(size of arr)/


>>> arr <- newArray (1,10) 0 :: IO (IOArray Int Int)
    -- Produces a 1 based array with 10 elements all set to 0.
>>> updateElems arr (+ 10)
    -- Updates all elements to 10

-}
INLINE(updateElems)
updateElems :: (MArray a e m, Ix i)
    => (e -> e) -- ^ Update function
    -> a i e    -- ^ The array
    -> m ()
updateElems f arr = do
    bnds@(_ , end') <- getBounds arr
    let !end = unsafeIndex bnds end'
    forM_ [0..end] (update arr f)


-- | The same as updateElems but taking a monadic function. /O(size of arr)/
INLINE(updateElemsM)
updateElemsM :: (MArray a e m, Ix i) => (e -> m e) -> a i e -> m ()
updateElemsM f arr = do
    bnds@(_ , end') <- getBounds arr
    let !end = unsafeIndex bnds end'
    forM_ [0..end] (updateM arr f)


-- | The same as updateElems, but also providing the index to the
-- mapping function. /O(size of arr)/
INLINE(updateElemsIx)
updateElemsIx :: (MArray a e m, Ix i) => (i -> e -> e) -> a i e -> m ()
updateElemsIx f arr = do
    bnds@(_ , end') <- getBounds arr
    let !end                    = unsafeIndex bnds end'

        -- Should be guaranteed that xs will be null when i = end+1
        go !i (x:xs) | i <= end = updateIx arr f i x >> go (i+1) xs
        go _ _                  = return ()
    go 0 (range bnds)


-- | The same updateElemsIx but taking a monadic function. /O(size of arr)/
INLINE(updateElemsIxM)
updateElemsIxM :: (MArray a e m, Ix i) => (i -> e -> m e) -> a i e -> m ()
updateElemsIxM f arr = do
    bnds@(_ , end') <- getBounds arr
    let !end                     = unsafeIndex bnds end'

        -- Should be guaranteed that xs will be null when i = end+1
        go !i (x:xs) | i <= end = updateIxM arr f i x >> go (i+1) xs
        go _ _                  = return ()
    go 0 (range bnds)

-- ======================
-- = updateElemsWithin* =
-- ======================

{-|
Takes an update function 'f' and a tuple of indicies '(start, finish)',
and applies the function to all elements returned by 'range (start, finish)'.

If this is a 2D array, then the area updated will be the box bounded by these elements,
and the rectangular prism area for a 3D array etc.

Throws an 'IndexOutOfBounds' exception if either of the indicies are out of bounds.
-}
INLINE(updateElemsWithin)
updateElemsWithin :: (MArray a e m, Ix i)
    => (e -> e) -- ^ Update function
    -> (i,i)    -- ^ The bounds within which to apply f. 
    -> a i e    -- ^ The array
    -> m ()
updateElemsWithin f (start, finish) arr = do
    bnds <- getBounds arr
    let !ok      = inRange bnds start && inRange bnds finish
        indicies = map (unsafeIndex bnds) $ range (start, finish)

    when (not ok) $ throw $ IndexOutOfBounds $ "Data.Array.Util updateElemsWithin"
    forM_ indicies (update arr f)


-- | The same as 'updateElemsWithin' but taking a monadic function.
-- 
-- Throws an 'IndexOutOfBounds' exception if either of the indicies are out of bounds.
INLINE(updateElemsWithinM)
updateElemsWithinM :: (MArray a e m, Ix i) => (e -> m e) -> (i,i) -> a i e -> m ()
updateElemsWithinM f (start, finish) arr = do
    bnds <- getBounds arr
    let !ok      = inRange bnds start && inRange bnds finish
        indicies = map (unsafeIndex bnds) $ range (start, finish)

    when (not ok) $ throw $ IndexOutOfBounds $ "Data.Array.Util updateElemsWithin"
    forM_ indicies (updateM arr f)


-- | Takes an update function 'f' and a tuple of indicies '(start, finish)'
-- , and applies the function to all elements returned by range (start, finish).
-- 
-- Throws an 'IndexOutOfBounds' exception if either of the indicies are out of bounds.
INLINE(updateElemsWithinIx)
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


-- | The same as 'updateElemsWithinIx' but taking a monadic function.
-- 
-- Throws an 'IndexOutOfBounds' exception if either of the indicies are out of bounds.
INLINE(updateElemsWithinIxM)
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

-- ==================
-- = updateElemsOn* =
-- ==================

{-|
Takes a mapping function, and a list of indicies to mutate.

Throws an 'IndexOutOfBounds' exception if any of the indicies are
out of bounds. In this case the array will be left unmutated.
/O(length xs)/
-}
INLINE(updateElemsOn)
updateElemsOn :: (MArray a e m, Ix i)
    => (e -> e) -- ^ Update function
    -> [i]      -- ^ A list of indicies to update
    -> a i e    -- ^ The array
    -> m ()
updateElemsOn f xs arr = do
    bnds <- getBounds arr
    let !ok      = all (inRange bnds) xs
        toOffset = unsafeIndex bnds

    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElems'")
    forM_ (map toOffset xs) (update arr f)


-- | Takes a mapping function, and a list of indicies to mutate.
-- Throws an 'IndexOutOfBounds' exception if any of the indicies are
-- out of bounds. In this case the array will be left unmutated.
-- /O(length xs)/
INLINE(updateElemsOnM)
updateElemsOnM :: (MArray a e m, Ix i) => (e -> m e) -> [i] -> a i e -> m ()
updateElemsOnM f xs arr = do
    bnds <- getBounds arr
    let !ok      = all (inRange bnds) xs
        toOffset = unsafeIndex bnds

    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElems'")
    forM_ (map toOffset xs) (updateM arr f)


-- | Takes a mapping function which takes an index, and a list of indicies
-- to mutate. Throws 'IndexOutOfBounds' exception as 'updateElems'' does.
-- /O(length xs)/
INLINE(updateElemsIxOn)
updateElemsIxOn :: (MArray a e m, Ix i) => (i -> e -> e) -> [i] -> a i e -> m ()
updateElemsIxOn f indexes arr = do
    bnds@(_, hi) <- getBounds arr
    let toOffset           = unsafeIndex bnds
        ixs                = map toOffset indexes
        end                = toOffset hi
        !ok                = all (\x -> x >= 0 && x <= end) ixs

        go (!i:is) (x:xs)  = updateIx arr f i x >> go is xs
        go _ _             = return ()

    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElemsIx'")
    go ixs indexes


-- | Takes a mapping function which takes an index, and a list of indicies
-- to mutate. /O(length xs)/
-- 
-- Throws 'IndexOutOfBounds' exception as 'updateElems'' does.
INLINE(updateElemsIxOnM)
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


-- ======================
-- = updateElemsSlice* =
-- ======================

{- $Slice
/Note the difference between these functions and updateElemsWithin./
These functions will update every element who that holds this property:

> f x = index (start,end) start <= ix && ix <= index (start,end) end
>   where ix = index (start, end) x

For example:

>>> arr <- newArray ((1,1),(5,5)) 0 :: IO (IOArray Int Int)
    -- Produces aa 2D array with 25 elements all set to 0.
>>> updateElemsSlice arr ((2,4),(3,5)) (+ 10)
    -- Updates elements at indexes [(2,4),(2,5),(3,1),(3,2),(3,3),(3,4),(3,5)] to 10

\*Ix versions are not included, because there's no easy way to map from
an Int to an element in a particular bounds.

All of these functions may throw 'IndexOutOfBounds' exceptions.
-}

{-|
updateElemsSlice mutates every element in an array between a start
index and an end index. /O(size of arr)/


>>> arr <- newArray (1,10) 0 :: IO (IOArray Int Int)
    -- Produces a 1 based array with 10 elements all set to 0.
>>> updateElemsSlice arr (2,4) (+ 10)
    -- Updates elements at indexes 2, 3 and 4 to 10


-}
INLINE(updateElemsSlice)
updateElemsSlice :: (MArray a e m, Ix i)
    => (e -> e) -- ^ Update function
    -> (i,i)    -- ^ The start and end of the region to update
    -> a i e    -- ^ The array
    -> m ()
updateElemsSlice f (start', finish') arr = do
    bnds@(_ , end') <- getBounds arr
    let !end    = unsafeIndex bnds end'
        !start  = unsafeIndex bnds start'
        !finish = unsafeIndex bnds finish'
        !ok     = start >= 0 && start <= end && finish >= 0 && finish <= end

    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElemsSlice")
    forM_ [start..finish] (update arr f)


-- | The same as updateElems but taking a monadic function. /O(size of arr)/
INLINE(updateElemsSliceM)
updateElemsSliceM :: (MArray a e m, Ix i) => (e -> m e) -> (i,i) -> a i e -> m ()
updateElemsSliceM f (start', finish') arr = do
    bnds@(_ , end') <- getBounds arr
    let !end    = unsafeIndex bnds end'
        !start  = unsafeIndex bnds start'
        !finish = unsafeIndex bnds finish'
        !ok     = start >= 0 && start <= end && finish >= 0 && finish <= end

    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElemsSlice")
    forM_ [start..finish] (updateM arr f)


-- =====================
-- = Utility functions =
-- =====================

{-# INLINE update #-}
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

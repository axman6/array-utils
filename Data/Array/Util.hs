{-# LANGUAGE BangPatterns #-}
module Data.Array.Util
    ( mapElems
    , mapElemsIx
    , mapElems'
    , mapElemsIx'
    ) where

import Data.Array.Base
import Data.Array.MArray
import Control.Monad

{-# INLINE mapElems #-}
-- | mapElems mutates every element in an array while avoiding
-- all bounds checks. /O(size of arr)/
mapElems :: (MArray a e m, Ix i) => (e -> e) -> a i e -> m ()
mapElems f arr = do
    (start, end') <- getBounds arr
    let !end = index (start, end') end'
        go !i | i <= end = do
            e <- unsafeRead arr i
            unsafeWrite arr i $! (f e)
            go (i+1)
        go _  = return ()
    go 0



{-# INLINE mapElemsIx #-}
-- | The same as mapElems, but also providing the index to the
-- mapping function. /O(size of arr)/
mapElemsIx :: (MArray a e m, Ix i) => (i -> e -> e) -> a i e -> m ()
mapElemsIx f arr =do
    bnds@(start, end') <- getBounds arr
    let !end = index (start, end') end'
        -- Should be guaranteed that xs will be null when i = end+1
        go !i (!x:xs) | i <= end = do
            e <- unsafeRead arr i
            unsafeWrite arr i $! f x e
            go (i+1) xs
        go _ _ = return ()
    go 0 (range bnds)

{-# INLINE mapElems' #-}
-- | Takes a mapping function, and a list of indicies to mutate.
-- Returns True if the muration occured, and False if the indicies
-- are out of range. /O(length xs)/
mapElems' :: (MArray a e m, Ix i) => (e -> e) -> [i] -> a i e -> m Bool
mapElems' f xs arr = do
    bnds@(lo, hi) <- getBounds arr
    let !ok = all (inRange bnds) xs
        toOffset = index (lo, hi)
        go (!i:is) = do
            e <- unsafeRead arr i
            unsafeWrite arr i $! f e
            go is
        go _ = return ()
    when ok $ go (map toOffset xs)
    return ok


{-# INLINE mapElemsIx' #-}
-- | Takes a mapping function which takes an index, and a list of indicies
-- to mutate. Returns True if the muration occured, and False if the indicies
-- are out of range. /O(length xs)/
mapElemsIx' :: (MArray a e m, Ix i) => (i -> e -> e) -> [i] -> a i e -> m Bool
mapElemsIx' f xs arr = do
    bnds@(lo, hi) <- getBounds arr
    let !ok =  all (inRange bnds) xs
        toOffset = index (lo, hi)
        ixs = map toOffset xs
        go (!i:is) (!i':is') = do
            e <- unsafeRead arr i
            unsafeWrite arr i $! f i' e
            go is is'
        go _ _ = return ()
    when ok $ go ixs xs
    return ok

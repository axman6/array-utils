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

{-# INLINE updateElems' #-}
-- | Takes a mapping function, and a list of indicies to mutate.
-- /O(length xs)/
updateElems' :: (MArray a e m, Ix i) => (e -> e) -> [i] -> a i e -> m ()
updateElems' f xs arr = do
    bnds <- getBounds arr
    let !ok = all (inRange bnds) xs
        toOffset = index bnds
    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElems': Index out of bounds")
    forM_ (map toOffset xs) $ \i -> unsafeRead arr i >>= unsafeWrite arr i . f


{-# INLINE updateElemsIx' #-}
-- | Takes a mapping function which takes an index, and a list of indicies
-- to mutate. /O(length xs)/
updateElemsIx' :: (MArray a e m, Ix i) => (i -> e -> e) -> [i] -> a i e -> m ()
updateElemsIx' f xs arr = do
    bnds@(lo, hi) <- getBounds arr
    let !ok =  all (inRange bnds) xs
        toOffset = index (lo, hi)
        ixs = map toOffset xs
        go (!i:is) (!i':is') = do
            e <- unsafeRead arr i
            unsafeWrite arr i $! f i' e
            go is is'
        go _ _ = return ()
    when (not ok) $ throw (IndexOutOfBounds $ "Data.Array.Util updateElemsIx': Index out of bounds")
    go ixs xs

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.Cyclic where

class (Eq c) => Cyclic c where
    ccw :: c -> c
    cw :: c -> c

    moveCounterclockwise :: (Integral i) => c -> i -> c
    x `moveCounterclockwise` 0 = x
    x `moveCounterclockwise` n
        | n > 0 = (ccw x) `moveCounterclockwise` (n - 1)
        | otherwise = (cw x) `moveCounterclockwise` (n + 1)

    moveClockwise :: (Integral i) => c -> i -> c
    x `moveClockwise` 0 = x
    x `moveClockwise` n
        | n > 0 = (cw x) `moveClockwise` (n - 1)
        | otherwise = (ccw x) `moveClockwise` (n + 1)

    countCounterclockwise :: (Integral i) => c -> c -> i
    x `countCounterclockwise` y
        | x == y = 0
        | otherwise = 1 + (ccw x) `countCounterclockwise` y
   
    countClockwise :: (Integral i) => c -> c -> i
    x `countClockwise` y 
        | x == y = 0
        | otherwise = 1 + (cw x) `countClockwise` y

    -- By convention, LT means clockwise, RT counterclockwise, and EQ either
    cyclicCompare :: c -> c -> c -> Ordering
    
-- By convention, counterclockwise is leftward and clockwise is rightward
instance (Bounded c, Enum c, Ord c) => Cyclic c where
    ccw x
        | x == minBound = maxBound
        | otherwise = pred x
    cw x 
        | x == maxBound = minBound
        | otherwise = succ x
    cyclicCompare x y z = doCompare (compare x y) (compare y z) (compare z x) where
        doCompare LT LT LT = GT -- x < y < z
        doCompare GT LT GT = GT -- y < x < z
        doCompare LT GT GT = GT -- x < z < y
        -- the opposites of the above
        doCompare GT GT GT = LT
        doCompare LT GT LT = LT
        doCompare GT LT LT = LT
        doCompare _ _ _ = EQ

-- More efficient verisons of the move and count functions,
-- assuming that the size of c is at most maxBound::Int
moveCCW :: (Bounded c, Enum c, Ord c, Cyclic c) => c -> Int -> c
x `moveCCW` n = toEnum ((fromEnum x - n) `mod` (fromEnum (maxBound `asTypeOf` x) + 1))

moveCW :: (Bounded c, Enum c, Ord c, Cyclic c) => c -> Int -> c
x `moveCW` n = toEnum ((fromEnum x + n) `mod` (fromEnum (maxBound `asTypeOf` x) + 1))

countCCW :: (Bounded c, Enum c, Ord c, Cyclic c) => c -> c -> Int
x `countCCW` y = (fromEnum x - fromEnum y) `mod` (fromEnum (maxBound `asTypeOf` x) + 1)

countCW :: (Bounded c, Enum c, Ord c, Cyclic c) => c -> c -> Int
x `countCW` y = (fromEnum y - fromEnum x) `mod` (fromEnum (maxBound `asTypeOf` x) + 1)


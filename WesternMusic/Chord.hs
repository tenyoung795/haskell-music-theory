{-# LANGUAGE FlexibleInstances,
    UndecidableInstances,
    MultiParamTypeClasses,
    FunctionalDependencies #-}
module WesternMusic.Chord where

import Control.Monad.Instances
import Data.Either
import Data.IndexError
import Data.List
import WesternMusic.Enharmonic
import WesternMusic.Tonal
import WesternMusic.Pitch
import WesternMusic.Interval

class (Intervalable i, Integral j) => Chord c i j | c -> i, c -> j where
    quality :: (Integral k) => c -> k -> Either (IndexError k) (Quality j)
    quality chord 0 = Right $ firstQuality chord
    quality chord 1 = Right $ secondQuality chord
    quality chord n
        | n < 0 = Left NegativeIndex
        | n >= numQ = Left $ OverIndex numQ where
            numQ = numQualities chord

    firstQuality :: c -> Quality j
    secondQuality :: c -> Quality j

    qualities :: c -> [Quality j]
    qualities chord = rights $ map (quality chord) [0 .. numQualities chord]
    numQualities :: (Integral k) => c -> k

    member :: (Integral k) => c -> k -> Either (IndexError k) i
    member chord 0 = Right $ root chord
    member chord 1 = Right $ third chord
    member chord 2 = Right $ fifth chord
    member chord n = do
        q <- quality chord (n - 1)
        return $ root chord `upByInterval` Interval q (1 + 2 * n)

    root :: c -> i
    third :: c -> i
    third chord = root chord `upByInterval` Interval (firstQuality chord) 3
    fifth :: c -> i
    fifth chord = root chord `upByInterval` Interval (secondQuality chord) 5

    members :: c -> [i]
    members chord = rights $ map (member chord) [0 .. numMembers chord]
    numMembers :: (Integral k) => c -> k
    numMembers chord = 1 + numQualities chord

    inversion :: (Integral k) => c -> k

    note :: (Integral k) => c -> k -> Either (IndexError k) i
    note chord n
        | n < 0 = Left NegativeIndex
        | n >= numM = Left $ OverIndex numM
        | otherwise = Right $ members chord `genericIndex` n where
            numM = numMembers chord

    bass :: c -> i
    bass chord = members chord `genericIndex` inversion chord
    notes :: c -> [i]
    notes chord = b:(left ++ right) where
        (left, b:right) = inversion chord `genericSplitAt` members chord

instance (Chord c i j) => Enharmonic c where
    x `enharmonic` y = inversion x == inversion y && members x `enharmonicLists` members y where
        [] `enharmonicLists` [] = True
        (_:_) `enharmonicLists` [] = False
        [] `enharmonicLists` (_:_) = False
        (lh:lt) `enharmonicLists` (rh:rt) = lh `enharmonic` rh && lt `enharmonicLists` rt


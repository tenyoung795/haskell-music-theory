module WesternMusic.Chord where

import Data.Maybe
import qualified WesternMusic.Interval as I

class Chord c where
    quality :: (Integral i, Integral j) => c -> i -> Maybe (I.Quality j)
    quality chord 0 = Just $ firstQuality chord
    quality chord 1 = Just $ secondQuality chord
    quality chord n | n < 0 = Nothing

    firstQuality :: (Integral i) => c -> I.Quality i
    secondQuality :: (Integral i) => c -> I.Quality i
    numQualities :: (Integral i) => c -> i

    qualities :: (Integral i) => c -> [I.Quality i]
    qualities chord = mapMaybe (quality chord) [0 .. numQualities chord]

    member :: (I.Intervalable i, Integral j) => c -> i -> j -> Maybe i
    member chord root 0 = Just root
    member chord root 1 = Just $ third chord root
    member chord root 2 = Just $ fifth chord root
    member chord root n
        | n > 0 = do
            q <- quality chord (n - 1)
            return $ root `I.upByInterval` I.Interval q (1 + 2 * n)
        | otherwise = Nothing

    third :: (I.Intervalable i) => c -> i -> i
    third chord root = root `I.upByInterval` I.Interval (firstQuality chord) 3
    fifth :: (I.Intervalable i) => c -> i -> i
    fifth chord root = root `I.upByInterval` I.Interval (secondQuality chord) 5

    members :: (I.Intervalable i) => c -> i -> [i]
    members chord root = mapMaybe (member chord root) [0 .. 1 + numQualities chord]

    inversion :: (Integral i) => c -> i

    note :: (I.Intervalable i, Integral j) => c -> i -> j -> Maybe i
    bass :: (I.Intervalable i) => c -> i -> i
    notes :: (I.Intervalable i) => c -> i -> [i]


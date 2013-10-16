module WesternMusic.Interval where

-- This should be imported as qualified
-- as Major and Minor are overloaded concepts.

import Data.Cyclic
import WesternMusic.Enharmonic
import WesternMusic.Tonal
import WesternMusic.Pitch
import qualified WesternMusic.Scale.Major as Maj

class Invertible i where
    invert :: i -> i

-- The quality constructor is for neither minor nor major qualities,
-- including diminished, perfect, and augmented qualities.
data Quality i = Minor | Quality i | Major deriving(Eq)

diminished :: (Integral i) => Quality i
diminished = Quality (-1)

minor :: (Integral i) => Quality i
minor = Minor

perfect :: (Integral i) => Quality i
perfect = Quality 0

major :: (Integral i) => Quality i
major = Major

augmented :: (Integral i) => Quality i
augmented = Quality 1

instance (Integral i) => Ord (Quality i) where
    compare Minor Major = LT
    compare Major Minor = GT
    compare Minor (Quality n)
        | n >= 0 = LT
        | otherwise = GT
    compare (Quality n) Major
        | n <= 0 = LT
        | otherwise = GT
    compare (Quality n) (Quality m) = compare n m

instance (Integral i) => Invertible (Quality i) where
    invert Minor = Major
    invert Major = Minor
    invert (Quality n) = Quality (-n)

instance (Show i, Integral i) => Show (Quality i) where
    show Major = "M"
    show Minor = "m"
    show (Quality 0) = "P"
    show (Quality 1) = "A"
    show (Quality (-1)) = "D"
    show (Quality n)
        | n < 0 = show (-n) ++ "D"
        | otherwise = show n ++ "A"

data Interval qualifier num = Interval (Quality qualifier) num deriving(Eq)

simplify :: (Integral i, Integral j) => Interval i j -> Interval i j
-- as a special case, dim to perfect octaves are considered simple
simplify (Interval Minor 8) = Interval Minor 8 
simplify (Interval (Quality 0) 8) = Interval (Quality 0) 8 
simplify (Interval (Quality n) 8)
    | n < 0 = Interval (Quality n) 8
    | otherwise = Interval (Quality n) 1
simplify (Interval q n) = Interval q (((n - 1) `mod` 7) + 1)

instance (Integral i, Integral j) => Enharmonic (Interval i j) where
    x `enharmonic` y = semitones x == semitones y

instance (Integral i, Integral j) => Tonal (Interval i j) where
    semitones (Interval q n) = fromRational(toRational (12 * octaves) + simple) where
        (octaves, n') = (n - 1) `divMod` 7
        doNormal :: (Integral i) => Quality i -> i -> Rational
        doNormal Minor major = toRational(major - 1)
        doNormal Major major = toRational major
        doNormal (Quality 0) major = toRational major - 1/2
        doNormal (Quality r) major
            | r < 0 = toRational(major - 1 + r)
            | otherwise = toRational(major + r)

        normal = doNormal q
        
        doPerfect :: (Integral i) => Quality i -> i -> Rational
        doPerfect (Quality r) perfect = toRational(perfect + r)
        doPerfect Minor perfect = toRational perfect - 1/2
        doPerfect Major perfect = toRational perfect + 1/2

        perfect = doPerfect q

        doSimple 0 = perfect 0
        doSimple 1 = normal 2
        doSimple 2 = normal 4
        doSimple 3 = perfect 5
        doSimple 4 = perfect 7
        doSimple 5 = normal 9
        doSimple 6 = normal 11
        
        simple = doSimple n'

instance (Integral i, Integral j) => Invertible (Interval i j) where
    invert (Interval q n) = Interval (invert q) n' where
        doN 8 = 1
        doN n = 8 - ((n - 1) `mod` 7)
        n' = doN n

instance (Show i, Integral i, Show j, Integral j) => Show (Interval i j) where
    show (Interval q n) = show q ++ show n

class (Pitched p) => Intervalable p where
    ascInterval :: (Integral i, Integral j) => p -> p -> Interval i j
    desInterval :: (Integral i, Integral j) => p -> p -> Interval i j
    desInterval = flip ascInterval

    upByInterval :: (Integral i, Integral j) => p -> Interval i j -> p
    downByInterval :: (Integral i, Integral j) => p -> Interval i j -> p

instance (RealFrac r) => Intervalable (Class r) where
    -- Consult the major scale degree of the beginning note,
    -- then determine the quality based on the accidental.
    ascInterval (Class xl xa) (Class yl ya) = Interval quality (fromIntegral(n + 1)) where
        n = xl `countCW` yl
        normalQuality 0 = Major
        normalQuality (-1) = Minor
        normalQuality diff
            | diff < 0 = Quality (diff + 1)
            | otherwise = Quality diff
        
        perfectQuality = Quality
        
        Class _ acc = (Class xl xa) `Maj.degree` ((toEnum n)::Maj.Degree)

        doQuality 0 = perfectQuality
        doQuality 3 = perfectQuality
        doQuality 4 = perfectQuality
        doQuality _ = normalQuality

        quality = (doQuality n) (round (ya - acc))
    upByInterval (Class l a) (Interval q n) = Class l' a'' where
        n' = (n - 1) `mod` 7
        Class l' a' = (Class l a) `Maj.degree` ((toEnum (fromIntegral n'))::Maj.Degree)

        normal Minor = -1
        normal Major = 0
        normal (Quality x)
            | x < 0 = -1 + realToFrac x
            | x > 0 = realToFrac x
            | otherwise = -1/2

        perfect Minor = -1/2
        perfect Major = 1/2
        perfect (Quality x) = realToFrac x

        differ 0 = perfect
        differ 3 = perfect
        differ 4 = perfect
        differ _ = normal

        a'' = a' + differ n' q
    downByInterval c i = upByInterval c (invert i)



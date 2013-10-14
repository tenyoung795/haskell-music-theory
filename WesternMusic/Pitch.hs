{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module WesternMusic.Pitch where

import WesternMusic.Enharmonic
import WesternMusic.Tonal
import Data.List
import Data.Cyclic

flat :: (Integral i) => i
flat = -1

natural :: (Integral i) => i
natural = 0

sharp :: (Integral i) => i
sharp = 1

flatChar :: Char
flatChar = '♭'

sharpChar :: Char
sharpChar = '♯'

data Letter = C | D | E | F | G | A | B deriving(Eq, Enum, Ord, Bounded, Show)

class (Tonal t) => Pitched t where
    letter :: t -> Letter
    accidental :: (Integral i) => t -> i

instance Enharmonic Letter where
    x `enharmonic` y = x == y

instance Tonal Letter where
    semitones C = 0
    semitones D = 2
    semitones E = 4
    semitones F = 5
    semitones G = 7
    semitones A = 9
    semitones B = 11
    
instance Pitched Letter where
    letter l = l
    accidental _ = 0

data Class accidental = Class Letter accidental deriving(Eq)

instance (Integral i) => Enharmonic (Class i) where
    x `enharmonic` y = semitones x == semitones y

instance (Integral i) => Tonal (Class i) where
    semitones (Class t a) = fromRational(toRational(truncate(((semitones t)::Rational) + toRational a) `rem` 12))

instance (Integral i) => Pitched (Class i) where
    letter (Class l _) = l
    accidental (Class _ a) = fromIntegral a

instance (Integral i, Show i) => Show (Class i) where
    show (Class l 0) = show l
    show (Class l n)
        | n < 0 = show l ++ genericReplicate (-n) flatChar
        | otherwise = show l ++ sharps n where
            sharps 0 = ""
            sharps 1 = [sharpChar]
            sharps 2 = "x"
            sharps n
                | odd n = sharpChar:(genericReplicate ((n - 1) `div` 2) 'x')
                | otherwise = genericReplicate (n `div` 2) 'x'

data Pitch accidental octave = Pitch (Class accidental) octave deriving(Eq)

instance (Integral i, Integral j) => Enharmonic (Pitch i j) where
    x `enharmonic` y = semitones x == semitones y

instance (Integral i, Integral j) => Tonal (Pitch i j) where
    -- semitones from C4
    semitones (Pitch c o) = fromRational(12 * (toRational o - 4) + (semitones c)::Rational)

instance (Integral i, Integral j) => Pitched (Pitch i j) where
    letter (Pitch p _) = letter p
    accidental (Pitch p _) = accidental p

instance (Integral i, Show i, Show j) => Show (Pitch i j) where
    show (Pitch c o) = show c ++ show o


{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module WesternMusic.Pitch where

import WesternMusic.Enharmonic
import WesternMusic.Tonal
import Data.List
import Data.Cyclic

flat :: (RealFrac r) => r
flat = -1

natural :: (RealFrac r) => r
natural = 0

sharp :: (RealFrac r) => r
sharp = 1

doubleFlatChar :: Char
doubleFlatChar = '𝄫'

flatChar :: Char
flatChar = '♭'

halfFlatChar :: Char
halfFlatChar = '𝄳'

halfSharpChar :: Char
halfSharpChar = '𝄲'

sharpChar :: Char
sharpChar = '♯'

doubleSharpChar :: Char
doubleSharpChar = '𝄪'

data Letter = C | D | E | F | G | A | B deriving(Eq, Enum, Ord, Bounded, Show)

class (Tonal t) => Pitched t where
    letter :: t -> Letter
    accidental :: (RealFrac r) => t -> r

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

instance (RealFrac r) => Enharmonic (Class r) where
    x `enharmonic` y = semitones x == semitones y

instance (RealFrac r) => Tonal (Class r) where
    semitones (Class t a) = realToFrac (truncate (((semitones t) `asTypeOf` a) + a) `rem` 12)

instance (RealFrac r) => Pitched (Class r) where
    letter (Class l _) = l
    accidental (Class _ a) = realToFrac a

instance (RealFrac r, Show r) => Show (Class r) where
    show (Class l 0) = show l
    show (Class l n)
        | n < 0 = show l ++ flats (-n) 
        | otherwise = show l ++ sharps n where
            acc _ _ _ 0 = ""
            acc half _ _ 0.5 = [half]
            acc _ whole _ 1 = [whole]
            acc half whole _ 1.5 = [half, whole]
            acc _ _ double 2 = [double]
            acc half whole double n
                | n > 2 = (acc half whole double (n-2)) ++ [double]
                | 1 < n && n < 2 = '(':(show (n - 1)) ++ whole:')':[whole]            
                | otherwise = '(':(show n) ++ whole:")"

            flats = acc halfFlatChar flatChar doubleFlatChar
            sharps = acc halfSharpChar sharpChar doubleSharpChar

data Pitch accidental octave = Pitch (Class accidental) octave deriving(Eq)

instance (RealFrac r, Integral i) => Enharmonic (Pitch r i) where
    x `enharmonic` y = semitones x == semitones y

instance (RealFrac r, Integral i) => Tonal (Pitch r i) where
    -- semitones from C4
    semitones (Pitch c o) = fromIntegral(12 * (o - 4)) + (semitones c)

instance (RealFrac r, Integral i) => Pitched (Pitch r i) where
    letter (Pitch p _) = letter p
    accidental (Pitch p _) = accidental p

instance (RealFrac r, Show r, Show i) => Show (Pitch r i) where
    show (Pitch c o) = show c ++ show o


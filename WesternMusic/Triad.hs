module WesternMusic.Triad where

-- This should be imported as qualified
-- as Major and Minor are overloaded concepts.

import WesternMusic.Tonal
import WesternMusic.Pitch
import qualified WesternMusic.Interval as I
import Data.Char
import Data.Cyclic

data Quality = Diminished | Minor | Major | Augmented deriving(Eq, Ord, Enum, Show)

data Inversion = Root | First | Second deriving(Eq, Enum, Show)

data Triad pitch = Triad pitch Quality Inversion deriving(Eq)

root :: Triad p -> p
root (Triad p _ _) = p

third :: (I.Intervalable i) => Triad i -> i
third (Triad p q _) = p `I.upByInterval` (I.Interval (q' q) 3) where
    q' Augmented = I.Major
    q' Major = I.Major
    q' Minor = I.Minor
    q' Diminished = I.Minor

fifth :: (I.Intervalable i) => Triad i -> i
fifth (Triad p q _) = p `I.upByInterval` (I.Interval (q' q) 5) where
    q' Augmented = I.Major
    q' Major = I.Minor
    q' Minor = I.Major
    q' Diminished = I.Minor

figuredBass :: Inversion -> String
figuredBass Root = ""
figuredBass First = "6"
figuredBass Second = "6/4"

chord :: (I.Intervalable i) => Triad i -> [i]
chord t = doChord i where
    Triad _ _ i = t
    doChord Root = [root t, third t, fifth t]
    doChord First = [third t, root t, fifth t]
    doChord Second = [fifth t, root t, third t]

calculateTriad :: (I.Intervalable i) => i -> i -> i -> Maybe (Triad i)
calculateTriad x y z = doCalculateTriad (x `I.ascInterval` y) (y `I.ascInterval` z) (z `I.ascInterval` x) where
    doCalculateTriad (I.Interval I.Major 3) (I.Interval I.Minor 3) _ = Just (Triad x Major Root)
    doCalculateTriad (I.Interval I.Minor 3) (I.Interval I.Major 3) _ = Just (Triad x Minor Root)
    doCalculateTriad (I.Interval I.Minor 3) (I.Interval I.Minor 3) _ = Just (Triad x Diminished Root)
    doCalculateTriad (I.Interval I.Major 3) (I.Interval I.Major 3) _ = Just (Triad x Augmented Root)

    doCalculateTriad (I.Interval I.Minor 3) _ (I.Interval I.Major 3) = Just (Triad x Major First)
    doCalculateTriad (I.Interval I.Major 3) _ (I.Interval I.Minor 3) = Just (Triad x Minor First)
    doCalculateTriad (I.Interval I.Minor 3) _ (I.Interval I.Minor 3) = Just (Triad x Diminished First)
    doCalculateTriad (I.Interval I.Major 3) _ (I.Interval I.Major 3) = Just (Triad x Augmented First)

    doCalculateTriad _ (I.Interval I.Major 3) (I.Interval I.Minor 3) = Just (Triad x Major Second)
    doCalculateTriad _ (I.Interval I.Minor 3) (I.Interval I.Major 3) = Just (Triad x Minor Second)
    doCalculateTriad _ (I.Interval I.Minor 3) (I.Interval I.Minor 3) = Just (Triad x Diminished Second)
    doCalculateTriad _ (I.Interval I.Major 3) (I.Interval I.Major 3) = Just (Triad x Augmented Second)

    doCalculateTriad _ _ _ = Nothing

instance (I.Intervalable i, Show i) => Show (Triad i) where
    show t = show p ++ sign q ++ inversion i where
        Triad p q i = t
        sign Major = ""
        sign Minor = "-"
        sign Diminished = "°"
        sign Augmented = "+"
        inversion Root = ""
        inversion First = '/':(show (third t))
        inversion Second = '/':(show (fifth t))
    
figuredRoman :: (I.Intervalable i) => Triad i -> i -> String
figuredRoman (Triad p q i) tonic = roman ((letter tonic) `countCW` (letter p)) q ++ figuredBass i where
    roman 0 Major = "I"
    roman 1 Major = "II"
    roman 2 Major = "III"
    roman 3 Major = "IV"
    roman 4 Major = "V"
    roman 5 Major = "VI"
    roman 6 Major = "VII"
    roman 0 Minor = "i"
    roman 1 Minor = "ii"
    roman 2 Minor = "iii"
    roman 3 Minor = "iv"
    roman 4 Minor = "v"
    roman 5 Minor = "vi"
    roman 6 Minor = "vii"
    roman 0 Diminished = "i°"
    roman 1 Diminished = "ii°"
    roman 2 Diminished = "iii°"
    roman 3 Diminished = "iv°"
    roman 4 Diminished = "v°"
    roman 5 Diminished = "vi°"
    roman 6 Diminished = "vii°"
    roman 0 Augmented = "I+"
    roman 1 Augmented = "II+"
    roman 2 Augmented = "III+"
    roman 3 Augmented = "IV+"
    roman 4 Augmented = "V+"
    roman 5 Augmented = "VI+"
    roman 6 Augmented = "VII+"
    

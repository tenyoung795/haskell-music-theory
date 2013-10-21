{-# LANGUAGE FlexibleInstances,
    UndecidableInstances,
    MultiParamTypeClasses #-}
module WesternMusic.Chord.Triad where

import WesternMusic.Chord
import WesternMusic.Pitch
import WesternMusic.Interval
import Data.Char
import Data.Cyclic
import Data.IndexError

data Inversion = Root | First | Second deriving(Eq, Enum, Show)

data Triad intervalable qualifier = Triad intervalable (Quality qualifier) (Quality qualifier) Inversion deriving(Eq)

instance (Intervalable i, Integral j) => Chord (Triad i j) i j where
    firstQuality (Triad _ q _ _) = q
    secondQuality (Triad _ _ r _) = r
    numQualities _ = 2

    qualities (Triad _ q r _) = [q, r]

    root (Triad r _ _ _) = r

    members chord = map ($ chord) [root, third, fifth]

    inversion (Triad _ _ _ i) = fromIntegral $ fromEnum i

    note chord n
        | n < 0 = Left NegativeIndex
        | n >= 3 = Left $ OverIndex 3
        | otherwise = Right $ doNote inversion n where
            Triad root _ _ inversion = chord
            doNote Root 0 = root
            doNote Root 1 = third chord 
            doNote Root 2 = fifth chord
            doNote First 0 = third chord
            doNote First 1 = root
            doNote First 2 = fifth chord 
            doNote Second 0 = fifth chord
            doNote Second 1 = root
            doNote Second 2 = third chord

figuredBass :: Inversion -> String
figuredBass Root = ""
figuredBass First = "6"
figuredBass Second = "6/4"

calculateTriad :: (Intervalable i, Integral j) => i -> i -> i -> Maybe (Triad i j)
calculateTriad x y z = doCalculateTriad (x `ascInterval` y) (x `ascInterval` z) (z `ascInterval` y) where
    doCalculateTriad (Interval q 3) (Interval r 5) _ = Just (Triad x q r Root)
    doCalculateTriad _ (Interval r0 4) (Interval q 3) = Just (Triad x q (invert r0) First)
    doCalculateTriad _ (Interval r0 4) (Interval q0 6) = Just (Triad x (invert q0) (invert r0) Second)
    doCalculateTriad _ _ _ = Nothing

instance (Intervalable i, Show i, Integral j, Show j) => Show (Triad i j) where
    show t = show p ++ sign q r ++ inversion i where
        Triad p q r i = t
        sign Major (Quality 0) = ""
        sign Minor (Quality 0) = "-"
        sign Minor (Quality (-1)) = "°"
        sign Major (Quality 1) = "+"
        sign q0 r0 = show q0 ++ show r0
        inversion Root = ""
        inversion First = '/':(show (third t))
        inversion Second = '/':(show (fifth t))
    
figuredRoman :: (Intervalable i, Integral j) => Triad i j -> i -> String
figuredRoman (Triad p q r i) tonic = roman ((letter tonic) `countCW` (letter p)) q r ++ figuredBass i where
    roman 0 Major (Quality 0) = "I"
    roman 1 Major (Quality 0) = "II"
    roman 2 Major (Quality 0) = "III"
    roman 3 Major (Quality 0) = "IV"
    roman 4 Major (Quality 0) = "V"
    roman 5 Major (Quality 0) = "VI"
    roman 6 Major (Quality 0) = "VII"
    roman 0 Minor (Quality 0) = "i"
    roman 1 Minor (Quality 0) = "ii"
    roman 2 Minor (Quality 0) = "iii"
    roman 3 Minor (Quality 0) = "iv"
    roman 4 Minor (Quality 0) = "v"
    roman 5 Minor (Quality 0) = "vi"
    roman 6 Minor (Quality 0) = "vii"
    roman 0 Minor (Quality (-1)) = "i°"
    roman 1 Minor (Quality (-1)) = "ii°"
    roman 2 Minor (Quality (-1)) = "iii°"
    roman 3 Minor (Quality (-1)) = "iv°"
    roman 4 Minor (Quality (-1)) = "v°"
    roman 5 Minor (Quality (-1)) = "vi°"
    roman 6 Minor (Quality (-1)) = "vii°"
    roman 0 Major (Quality 1) = "I+"
    roman 1 Major (Quality 1) = "II+"
    roman 2 Major (Quality 1) = "III+"
    roman 3 Major (Quality 1) = "IV+"
    roman 4 Major (Quality 1) = "V+"
    roman 5 Major (Quality 1) = "VI+"
    roman 6 Major (Quality 1) = "VII+"
    

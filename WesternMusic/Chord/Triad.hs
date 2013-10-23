{-# LANGUAGE FlexibleInstances,
    UndecidableInstances,
    MultiParamTypeClasses #-}
module WesternMusic.Chord.Triad where

import WesternMusic.Chord
import WesternMusic.Pitch
import WesternMusic.Interval
import Data.Char(toLower)
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
            doNote Root _ = fifth chord
            doNote First 0 = third chord
            doNote First 1 = root
            doNote First _ = fifth chord 
            doNote Second 0 = fifth chord
            doNote Second 1 = root
            doNote Second _ = third chord

figuredBass :: Inversion -> String
figuredBass Root = ""
figuredBass First = "6"
figuredBass Second = "6/4"

calculateTriad :: (Intervalable i, Integral j) => i -> i -> i -> Maybe (Triad i j)
calculateTriad x y z = doCalculateTriad (x `ascInterval` y) (x `ascInterval` z) (z `ascInterval` y) where
    doCalculateTriad (Interval q 3) (Interval r 5) _ = Just (Triad x q r Root)
    doCalculateTriad (Interval q0 6) _ (Interval r0 4) = Just (Triad y (invert q0) (invert r0) First)
    doCalculateTriad (Interval r0 4) _ (Interval q0 6) = Just (Triad y (invert q0) (invert r0) Second)
    doCalculateTriad _ _ _ = Nothing

instance (Intervalable i, Show i, Integral j, Show j) => Show (Triad i j) where
    show t = show p ++ sign q r ++ inv i where
        Triad p q r i = t
        sign Major (Quality 0) = ""
        sign Minor (Quality 0) = "-"
        sign Minor (Quality (-1)) = "°"
        sign Major (Quality 1) = "+"
        sign q0 r0 = show q0 ++ show r0
        inv Root = ""
        inv First = '/':show (third t)
        inv Second = '/':show (fifth t)
    
figuredRoman :: (Intervalable i, Integral j, Show j) => Triad i j -> i -> String
figuredRoman (Triad p q r i) tonic = roman q r (letter tonic `countCW` letter p) ++ figuredBass i where
    rom 0 = "I"
    rom 1 = "II"
    rom 2 = "III"
    rom 3 = "IV"
    rom 4 = "V"
    rom 5 = "VI"
    rom 6 = "VII"
    rom _ = ""
    roman Major (Quality 0) n0 = rom n0
    roman Minor (Quality 0) n0 = map toLower (rom n0)
    roman Minor (Quality (-1)) n0 = (map toLower . rom) n0 ++ "°"
    roman Major (Quality 1) n0 = rom n0 ++ "+"
    roman q0 Major n0 = rom n0 ++ show q0 ++ show Major
    roman q0 Minor n0 = (map toLower . rom) n0 ++ show q0 ++ show Minor
    roman (Quality q0) (Quality 0) n0 = (trans . rom) n0 ++ show (Quality q0) ++ show (Quality 0) where
        trans
            | q0 > 0 = id
            | otherwise = map toLower
    roman q0 (Quality r0) n0 = (trans . rom) n0 ++ show q0 ++ show (Quality r0) where
        trans
            | r0 >= 0 = id
            | otherwise = map toLower
 

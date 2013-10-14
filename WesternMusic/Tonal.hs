module WesternMusic.Tonal where

import WesternMusic.Enharmonic

class (Enharmonic e) => Tonal e where
    semitones :: (RealFrac f) => e -> f


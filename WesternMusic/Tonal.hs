module WesternMusic.Tonal where

class Tonal t where
    semitones :: (RealFrac f) => t -> f
    enharmonic :: t -> t -> Bool
    x `enharmonic` y = (semitones x) == (semitones y)


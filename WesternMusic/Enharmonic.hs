module WesternMusic.Enharmonic where

class Enharmonic e where
    enharmonic :: e -> e -> Bool


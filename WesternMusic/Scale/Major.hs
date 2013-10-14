module WesternMusic.Scale.Major where

-- To be imported qualified to not clash with Minor

import Data.Cyclic
import WesternMusic.Pitch

data Degree = Tonic | Supertonic | Mediant | Subdominant | Dominant | Submediant | LeadingTone deriving(Eq, Ord, Enum, Bounded, Show)

class (Pitched p) => Scaled p where
    degree :: p -> Degree -> p
    scale :: p -> [p]
    scale x = map (degree x) [Tonic ..]

instance (Integral i) => Scaled (Class i) where
    degree (Class l a) d = Class l' (d' + a) where
        (l', d') = natDegree l d
        natDegree F Subdominant = (B, flat)
        natDegree F d = ((F `moveCW` (fromEnum d)), natural)

        natDegree C d = ((C `moveCW` (fromEnum d)), natural)

        natDegree G LeadingTone = (F, sharp)
        natDegree G d = ((G `moveCW` (fromEnum d)), natural)

        natDegree D Mediant = (F, sharp)
        natDegree D LeadingTone = (C, sharp)
        natDegree D d = ((D `moveCW` (fromEnum d)), natural)

        natDegree A Mediant = (C, sharp)
        natDegree A Submediant = (F, sharp)
        natDegree A LeadingTone = (G, sharp)
        natDegree A d = ((A `moveCW` (fromEnum d)), natural)

        natDegree E Tonic = (E, natural)
        natDegree E Subdominant = (A, natural)
        natDegree E Dominant = (B, natural)
        natDegree E d = ((E `moveCW` (fromEnum d)), sharp)

        natDegree B Tonic = (B, natural)
        natDegree B Subdominant = (E, natural)
        natDegree B d = ((B `moveCW` (fromEnum d)), sharp)


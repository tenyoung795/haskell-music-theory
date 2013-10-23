module Data.IndexError where

data IndexError i = NegativeIndex | OverIndex i deriving (Eq, Show)


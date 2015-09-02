module Data where

data Pixel = Pixel Int Int

type Snake = [Pixel]

data Game = Game 
    { snake :: Snake
    , direction :: Direction }

data Direction = Right | Left | Up | Down
    deriving (Eq)


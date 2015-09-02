module Const where

side :: Int
side = 20

sideF :: Float
sideF = fromIntegral side

vBlocks :: Int
vBlocks = 20

hBlocks :: Int
hBlocks = 20

windowHeight :: Int
windowHeight = vBlocks*side

windowWidth :: Int
windowWidth = hBlocks*side

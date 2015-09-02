module Const where

side :: Int
side = 20

sideF :: Float
sideF = fromIntegral side

wBlocks :: Int
wBlocks = 20

hBlocks :: Int
hBlocks = 20

windowHeight :: Int
windowHeight = hBlocks*side

windowWidth :: Int
windowWidth = wBlocks*side

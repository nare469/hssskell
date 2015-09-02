import Const
import Graphics.Gloss

--data Definitions
data Pixel = Pixel Int Int

data Snake = [Pixel]

--Shifting origin to bottom left corner
translateHeight :: Float
translateHeight = fromIntegral (side - windowHeight)/2

translateWidth :: Float
translateWidth = fromIntegral (side - windowWidth)/2

--rendering the data
drawPixelOrigin :: Picture
drawPixelOrigin = translate translateWidth translateHeight $ rectangleSolid sideF sideF

drawPixel :: Pixel -> Picture
drawPixel (Pixel x y) = translate (fromIntegral side*x) (fromIntegral side*y) drawPixelOrigin

drawSnake :: Snake -> [Picture]
drawSnake = map drawPixel

translatePixel :: Int -> Int -> Pixel -> Pixel
translatePixel dx dy (Pixel x y) = Pixel (x+dx) (y+dy)

frame = pictures [
    translatePixel 1 2 pixel,
    translatePixel 4 4 pixel ]

window :: Display
window = InWindow "Snake" (windowWidth, windowHeight) (0, 0)

main :: IO ()
main = display window white frame
